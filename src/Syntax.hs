



module Syntax where

type Name = String

data Expr
  = Float   Double
  | BinOp   Op Expr Expr
  | Var     Name
  | Call    Name [Expr]
  deriving (Eq, Ord, Show)

data Decl
  = Function Name [Name] Expr
  | Extern   Name [Name]
  | Plain    Expr
  deriving (Eq, Ord, Show)

data Prog
  = Prog [Decl]
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)
