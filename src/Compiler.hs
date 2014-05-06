{-# OPTIONS_GHC -fdefer-type-errors #-}
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
import LLVM.General.AST.Global as T
import Syntax as S

type Message = String

type ExprM =
  ReaderT (Map S.Name Operand) (
  StateT Word (
  WriterT [Named Instruction] (
  ErrorT Message
    Identity)))

runExprM :: ExprM a -> Either Message (a, [Named Instruction])
runExprM m = runIdentity $
             runErrorT $
             runWriterT $
             flip evalStateT 0 $
             flip runReaderT (singleton "test" (ConstantOperand (GlobalReference (T.Name "test")))) $ 
             m

embedInModule :: Operand -> [Named Instruction] -> Module
embedInModule ref instrs =
  let
    basicBlock = BasicBlock (T.Name "entry") instrs (Do (Ret (Just ref) []))
    function   = functionDefaults {
                   name        = T.Name "test"
                 , returnType  = FloatingPointType 64 IEEE
                 , parameters  = ([], False)
                 , basicBlocks = [basicBlock]
                 }
  in
    defaultModule {
      moduleName        = "Test"
    , moduleDefinitions = [GlobalDefinition function]
    }

incrCounter :: ExprM Word
incrCounter = do
  ctr <- get
  put (ctr + 1)
  return ctr

compileExpr' :: Expr -> ExprM Operand
compileExpr' e = case e of
  S.Float d        -> return (ConstantOperand (C.Float (Double d)))
  S.BinOp op e1 e2 -> do
                        o1 <- compileExpr' e1
                        o2 <- compileExpr' e2
                        genInstr (compileOp op o1 o2 [])
  S.Var n           -> do
                         symbols <- ask
                         case M.lookup n symbols of
                           Nothing -> throwError $ "unknown variable: " ++ n
                           Just o  -> return o
  S.Call n exprs    -> do
                         symbols <- ask
                         case M.lookup n symbols of
                           Nothing -> throwError "unknown functions"
                           Just o  -> do
                             os  <- mapM compileExpr' exprs
                             genInstr (call o os)

genInstr :: Instruction -> ExprM Operand
genInstr instr = do
  ctr <- incrCounter
  let newname = UnName ctr
  tell [newname := instr]
  return (LocalReference newname)

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


compileExpr e symbols counter = case e of
  S.Var n           -> case M.lookup n symbols of
                         Nothing -> Left "unknown variable"
                         Just o  -> Right (o, counter, [])
  S.Call n expres   -> case M.lookup n symbols of
                         Nothing -> Left "unknown function"
                         Just o  -> undefined

compileOp :: Op -> Operand -> Operand -> InstructionMetadata -> Instruction
compileOp op = case op of
  Plus   -> T.FAdd
  Minus  -> T.FSub
  Times  -> T.FMul
  Divide -> T.FDiv

