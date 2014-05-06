module Parser where

import Control.Applicative hiding ((<|>), many)
import Control.Monad.Identity
import Syntax
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token as Tok
import Text.Parsec.Expr as Ex
import Text.Parsec.String

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops   = ["+", "*", "-", "/", ";", ","]
    names = ["def", "extern"]
    style = emptyDef
             { Tok.commentLine     = "#"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames   = names
             }

binary :: String 
       -> Op
       -> Ex.Assoc
       -> Ex.Operator String () Identity Expr
binary s f assoc =
  Ex.Infix (reservedOp lexer s >> return (BinOp f)) assoc

table :: OperatorTable String () Identity Expr
table =
  [ [ binary "*" Times Ex.AssocLeft
    , binary "/" Divide Ex.AssocLeft
    ]
  , [ binary "+" Plus Ex.AssocLeft
    , binary "-" Minus Ex.AssocLeft
    ]
  , [ binary "<" LessThan Ex.AssocNone
    ]
  ]

exprLeaf :: Parser Expr
exprLeaf = 
      try (Float               <$> float lexer)
  <|>      Float . fromInteger <$> integer lexer
  <|> try (Call                <$> identifier lexer
                               <*> parens lexer (commaSep lexer expr)
          )
  <|>      Var                 <$> identifier lexer
  <|>                              parens lexer expr

expr :: Parser Expr
expr = buildExpressionParser table exprLeaf

decl :: Parser Decl
decl =
      Function <$  reserved lexer "def"
               <*> identifier lexer
               <*> parens lexer (many (identifier lexer))
               <*> expr
  <|> Extern   <$  reserved lexer "extern"
               <*> identifier lexer
               <*> parens lexer (many (identifier lexer))
  <|> Plain    <$> expr

prog :: Parser Prog
prog = Prog <$> semiSep lexer decl

parseExpr = parse (expr <* eof) "interactive"
