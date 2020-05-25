module Parser.DatalogParser.Expr
  ( aExpr, bPredExpr
  , term, predicateSymbol
  , rule
  ) where

import Text.Megaparsec

import Control.Monad.Combinators.Expr

import Data.Void (Void)
import Data.Foldable

import Parser.DatalogParser.Lexer
import Expr
import qualified Rule as R
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
  <|> predicate

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
aTerm = try term <|> parens aExpr

term :: Parser (Expr DBVar)
term = asum
  [ try $ Var . Free <$> variable 
  , try $ ConstStr   <$> predicateSymbol
  , try $ ConstNum   <$> signedInteger
  ]

-----------------------
-- Helper functions
-----------------------

binary :: String -> (a -> a -> a) -> Operator Parser a
binary n f = InfixL  (f <$ symbol n)

prefix :: String -> (a -> a) -> Operator Parser a
prefix n f = Prefix  (f <$ symbol n)

predicate :: Parser (Expr DBVar)
predicate = Pred <$> identifier <*> (parens $ sepBy1 term comma)

rule :: Parser R.Rule
rule = 
  do
    psym <- predicateSymbol
    terms <- option [] . parens $ sepBy1 term comma
    return $ R.fact psym terms

opParse :: BinOp -> String -> Parser (Expr DBVar)
opParse op s = 
  do
    expr <- try $ Binary op <$> aExpr <*> (symbol s *> aExpr)
    return $ expr

