module Main where

import Control.Monad.Trans.Error
import LLVM.General.AST as T
import LLVM.General.AST.Constant as T
import LLVM.General.AST.Global as T
import LLVM.General.AST.CallingConvention as T
import LLVM.General as L
import LLVM.General.Context as L
import LLVM.General.PassManager as L
import Syntax as S
import Compiler as C
import Parser as P
import System.Environment

doAll :: String -> IO ()
doAll str = do
  case parseExpr str of
    Left err   -> fail (show err)
    Right expr ->
      case runExprM $ compileExpr' expr of
        Left err          -> fail err
        Right (o, instrs) -> compile (embedInModule o instrs)

compile :: T.Module -> IO ()
compile ast =
  withContext $ \ ctx -> do
    r <- runErrorT $
           withModuleFromAST ctx ast $ \ m ->
           withPassManager (defaultPassSetSpec { transforms = [] }) $ \ pm -> do
             runPassManager pm m
             moduleLLVMAssembly m
    case r of
      Left  e -> putStr e
      Right p -> putStr p

main :: IO ()
main = do
  args <- getArgs
  doAll (unwords args)

defs :: [T.Definition]
defs =
  [ T.GlobalDefinition
      (functionDefaults { name = Name "putchar"
                        , returnType = IntegerType 32
                        , parameters = 
                            ([ Parameter (IntegerType 32) (Name "x") [] ], False)
                        })
  , T.GlobalDefinition
      (functionDefaults { name = Name "main"
                        , returnType = VoidType
                        , parameters = ([], False)
                        , basicBlocks =
                            [ BasicBlock
                                (T.UnName 0)
                                [Do (T.Call False T.C []
                                    (Right (ConstantOperand (GlobalReference (T.Name "putchar")))) 
                                    [(ConstantOperand (Int 32 42), [])] [] [])]
                                (Do (Ret Nothing []))
                            ]
                        })
  ]

