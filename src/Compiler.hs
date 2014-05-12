module Compiler where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.Identity
import Data.Map as M
import Data.Word
import LLVM.General.AST as T
import LLVM.General.AST.Constant as C
import LLVM.General.AST.CallingConvention as T
import LLVM.General.AST.Float as T
import qualified LLVM.General.AST.FloatingPointPredicate as T
import LLVM.General.AST.Global as T
import Syntax as S

-- | Type synonym for error messages
type Message = String

-- | Maps known source identifiers (function parameters and
-- function names) to LLVM operands.
type SymbolTable = Map S.Name Operand

-- | Expression compiler monad
type ExprM =
  ReaderT (Map S.Name Operand) (
  StateT Word (
  WriterT [Named Instruction] (
  ErrorT Message
    Identity)))

-- | Runs the expression monad by supplying initial values.
runExprM :: ExprM a -> Either Message (a, [Named Instruction])
runExprM m = runIdentity $
             runErrorT $
             runWriterT $
             flip evalStateT 0 $
             flip runReaderT (singleton "test" (ConstantOperand (GlobalReference (T.Name "test")))) $ -- just a hack
             m

-- | Helper function in the expression monad: get the current counter
-- value and increment it by one.
incrCounter :: ExprM Word
incrCounter = do
  ctr <- get
  put (ctr + 1)
  return ctr

-- | Helper function in the expression monad: look up a name in the
-- symbol table.
lookupSymbol :: S.Name               -- ^ name to look up
             -> ExprM a              -- ^ what to do on failure
             -> (Operand -> ExprM a) -- ^ what to do on success
             -> ExprM a
lookupSymbol n nothing just = do
  symbols <- ask
  case M.lookup n symbols of
    Nothing -> nothing
    Just o  -> just o

-- | Helper function in the expression monad: emit a new instruction;
-- return the operand that is a reference to the result.
emit :: Instruction -> ExprM Operand
emit instr = do
  ctr <- incrCounter
  let newname = UnName ctr
  tell [newname := instr]
  return (LocalReference newname)

-- | Compile an expression.
compileExpr :: Expr -> ExprM Operand
compileExpr e = case e of
  S.Float d        -> return (constDouble d)
  S.BinOp op e1 e2 -> do
                        o1 <- compileExpr e1
                        o2 <- compileExpr e2
                        compileOp op o1 o2
  S.Var n          -> lookupSymbol n
                        (throwError $ "unknown variable: " ++ n)
                        return
  S.Call n exprs   -> lookupSymbol n
                        (throwError $ "unknown function: " ++ n)
                        (\ o -> do
                                  os <- mapM compileExpr exprs
                                  emit (call o os)
                        )

-- | Compile an operator.
compileOp :: Op -> Operand -> Operand -> ExprM Operand
compileOp op o1 o2 = case op of
  Plus     -> emit (T.FAdd o1 o2 [])
  Minus    -> emit (T.FSub o1 o2 [])
  Times    -> emit (T.FMul o1 o2 [])
  Divide   -> emit (T.FDiv o1 o2 [])
  LessThan -> do
                bool <- emit (T.FCmp T.ULT o1 o2 [])
                emit (T.UIToFP bool double [])

-- Helper function to generate a double constant
constDouble :: Double -> Operand
constDouble d = ConstantOperand (C.Float (Double d))

-- Helper function to generate a call instruction
call :: Operand -> [Operand] -> Instruction
call x rs =
  T.Call
    False  -- never a tail call
    C      -- C calling convention
    []
    (Right x)
    (zip rs (repeat []))
    []
    []

double :: Type
double = FloatingPointType 64 IEEE

-- | Compile a declaration.
compileDecl :: Decl -> Map S.Name Operand -> (Definition, Map S.Name Operand)
compileDecl (Extern nm parms) m = (GlobalDefinition func, insert nm operand m)
  where func = functionDefaults {
                   name        = T.Name nm
                 , returnType  = double
                 , parameters  = (Prelude.map namedParam parms, False)
                 }
        namedParam pnm = Parameter double (T.Name nm) []
        operand = ConstantOperand (GlobalReference (name func))
compileDecl (S.Function nm parms body) m = (GlobalDefinition func, insert nm operand m)
  where func = functionDefaults {
                   name        = T.Name nm
                 , returnType  = double
                 , parameters  = (Prelude.map namedParam parms, False)
                 , basicBlocks = [] -- FIXME
                 }
        namedParam pnm = Parameter double (T.Name nm) []
        operand = ConstantOperand (GlobalReference (name func))

-- | Embed a compiled function in a single function called "test"
-- which in turn is contained in a module called "Test".
embedInModule :: Operand -> [Named Instruction] -> Module
embedInModule ref instrs =
  let
    basicBlock = BasicBlock (T.Name "entry") instrs (Do (Ret (Just ref) []))
    function   = functionDefaults {
                   name        = T.Name "test"
                 , returnType  = double
                 , parameters  = ([], False)
                 , basicBlocks = [basicBlock]
                 }
  in
    defaultModule {
      moduleName        = "Test"
    , moduleDefinitions = [GlobalDefinition function]
    }

