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

type Parser = Parsec Void String

-----------------------
-- Predicate expressions
-----------------------

bPredExpr :: Parser Expr
bPredExpr = 
      opParse BLE "=<"
  <|> opParse BLT "<"
  <|> opParse BGE ">="
  <|> opParse BGT ">"
  <|> opParse BEQ "="
  <|> opParse BEQ "is"
  <|> predParse

-----------------------
-- Numeric expressions
-----------------------

aExprTable :: [[Operator Parser Expr]]
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

aExpr :: Parser Expr
aExpr = makeExprParser aTerm aExprTable

aTerm :: Parser Expr
aTerm = try term <|> parens aExpr

term :: Parser Expr
term = asum
  [ try $ Var . free <$> variable 
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

predParse :: Parser Expr
predParse = predicate <$> identifier <*> (parens $ sepBy1 term comma)

rule :: Parser R.Rule
rule = 
  do
    psym <- predicateSymbol
    terms <- option [] . parens $ sepBy1 term comma
    return $ R.fact psym terms

