module Parser.DatalogParser.Expr
  ( aExpr, bPredExpr
  , term, predicateSymbol
  , literal
  ) where

import Text.Megaparsec

import Control.Monad.Combinators.Expr

import Data.Void (Void)

import Parser.DatalogParser.Lexer
import Expr
import DBClause

type Parser = Parsec Void String

-----------------------
-- Predicate expressions
-----------------------

bPredExpr :: Parser (Expr DBVar)
bPredExpr = 
      opParse BLE "=<"
  <|> opParse BLT "<"
  <|> opParse BGE ">="
  <|> opParse BGT ">"
  <|> opParse BEQ "="
  <|> opParse BEQ "is"
  <|> (try $ literal)

-----------------------
-- Numeric expressions
-----------------------

aExprTable :: [[Operator Parser (Expr DBVar)]]
aExprTable = 
  [ [ prefix "-" $ Unary Neg
    ]
  , [ binary "\\/" $ Binary Max
    , binary "/\\" $ Binary Min
    ]
  , [ binary "*" $ Binary Mult
    , binary "/" $ Binary Div
    ]
  , [ binary "+" $ Binary Add
    , binary "-" $ Binary Sub
    ]
  ]

aExpr :: Parser (Expr DBVar)
aExpr = makeExprParser aTerm aExprTable

aTerm :: Parser (Expr DBVar)
aTerm = term <|> parens aExpr

term :: Parser (Expr DBVar)
term = 
      (try $ Var      <$> variable )
  <|> (try $ ConstStr <$> predicateSymbol)
  <|> (try $ ConstNum <$> signedInteger)
  <|> (fail $ "could not parse term")

-----------------------
-- Helper functions
-----------------------

binary :: String -> (a -> a -> a) -> Operator Parser a
binary n f = InfixL  (f <$ symbol n)

prefix :: String -> (a -> a) -> Operator Parser a
prefix n f = Prefix  (f <$ symbol n)

literal :: Parser (Expr DBVar)
literal = 
  do
    psym <- predicateSymbol
    terms <- option [] . parens $ sepBy1 term comma
    let terms' = terms
    return $ Pred psym terms'

opParse :: BinOp -> String -> Parser (Expr DBVar)
opParse op s = 
  do
    expr <- try $ Binary op <$> aExpr <*> (symbol s *> aExpr)
    return $ expr

