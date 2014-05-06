{-# OPTIONS_GHC -fdefer-type-errors #-}
module Compiler where

import Data.Map as M
import Data.Word
import LLVM.General.AST as T
import LLVM.General.AST.Constant as C
import LLVM.General.AST.Float as T
import Syntax as S

type Message = String

compileExpr :: Expr                -- ^ expression to compile
            -> Map S.Name Operand  -- ^ known symbols
            -> Word                -- ^ name counter
            -> Either Message
                 ( Operand             -- ^ reference to result
                 , Word                -- ^ new name counter
                 , [Named Instruction] -- ^ list of generated instructions
                 )
compileExpr e symbols counter = case e of
  S.Float d         -> Right (ConstantOperand (C.Float (Double d)), counter, [])
  S.BinOp op e1 e2  -> case compileExpr e1 symbols counter of
                         Left err -> Left err
                         Right (o1, counter1, instrs1) ->
                           case compileExpr e2 symbols counter1 of
                             Left err -> Left err
                             Right (o2, counter2, instrs2) ->
                               let
                                 newname = UnName counter2
                                 newref  = LocalReference newname
                                 opinstr = case op of
                                             Plus   -> T.FAdd o1 o2 []
                                             Minus  -> T.FSub o1 o2 []
                                             Times  -> T.FMul o1 o2 []
                                             Divide -> T.FDiv o1 o2 []
                                 nmdinstr = newname := opinstr
                               in Right (newref, counter2 + 1, instrs1 ++ instrs2 ++ [nmdinstr])

-- (%13, [%13 = fadd %7 %10])
                                 
  S.Var n           -> case M.lookup n symbols of
                         Nothing -> Left "unknown variable"
                         Just o  -> Right (o, counter, [])
  S.Call n expres   -> case M.lookup n symbols of
                         Nothing -> Left "unknown function"
                         Just o  -> undefined
