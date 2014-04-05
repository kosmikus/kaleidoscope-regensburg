{-# LANGUAGE DeriveFunctor #-}
module Compiler where

import Control.Applicative
import Control.Monad.Free
import LLVM.General.AST                   as T
import LLVM.General.AST.CallingConvention as T
import LLVM.General.AST.Constant          as C
import LLVM.General.AST.Float             as T

import Syntax as S

--------------------------------------------------------------------------

data FunctionCompilerOps a =
    Emit   Instruction (Operand -> a)
  | Access S.Name      (Operand -> a)
  deriving Functor

type FunctionCompiler = Free FunctionCompilerOps

emit :: Instruction -> FunctionCompiler Operand
emit i = liftF (Emit i id)

access :: S.Name -> FunctionCompiler Operand
access x = liftF (Access x id)

--------------------------------------------------------------------------

data ModuleCompilerOps a =
    Declare S.Name [S.Name]

type ModuleCompiler = Free ModuleCompilerOps

--------------------------------------------------------------------------

call :: S.Name -> [Operand] -> FunctionCompiler Operand
call x rs = emit $
  T.Call
    False  -- never a tail call
    C      -- C calling convention
    []
    (Right (ConstantOperand (GlobalReference (Name x)))) -- only global funs
    (zip rs (repeat []))
    []
    []

compileExpr :: Expr -> FunctionCompiler Operand
compileExpr e = case e of
  S.Float d        -> return (ConstantOperand (C.Float (Double d)))
  S.BinOp op e1 e2 -> do
    r1 <- compileExpr e1
    r2 <- compileExpr e2
    compileOp op r1 r2
  S.Var x          -> access x
  S.Call x es      -> do
    rs <- mapM compileExpr es
    call x rs

compileOp :: Op -> Operand -> Operand -> FunctionCompiler Operand
compileOp op r1 r2 = case op of
  Plus   -> emit (T.FAdd r1 r2 [])
  Minus  -> emit (T.FSub r1 r2 [])
  Times  -> emit (T.FMul r1 r2 [])
  Divide -> emit (T.FDiv r1 r2 [])

compileDecl :: Decl -> ModuleCompiler ()
compileDecl = undefined
