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
import Expr as E
import qualified Rule as R

type Parser = Parsec Void String

-----------------------
-- Predicate expressions
-----------------------

bPredExpr :: Parser Expr
bPredExpr = 
      (try $ opParse lessEqual    "=<")
  <|> (try $ opParse less         "<" )
  <|> (try $ opParse greaterEqual ">=")
  <|> (try $ opParse greater      ">" )
  <|> (try $ opParse equal        "=" )
  <|> (try $ opParse equal        "is")
  <|> predParse
  where 
    opParse :: (Expr -> Expr -> Expr) -> String -> Parser Expr
    opParse f s = f <$> aExpr <*> (symbol s *> aExpr)

-----------------------
-- Numeric expressions
-----------------------

aExprTable :: [[Operator Parser Expr]]
aExprTable = 
  [ [ prefix "-" eNeg
    ]
  , [ binary "\\/" eMax
    , binary "/\\" eMin
    ]
  , [ binary "*" eMul
    , binary "/" eDiv
    ]
  , [ binary "+" eAdd
    , binary "-" eSub
    ]
  ]

aExpr :: Parser Expr
aExpr = makeExprParser aTerm aExprTable

aTerm :: Parser Expr
aTerm = try term <|> parens aExpr

term :: Parser Expr
term = asum
  [ try $ var      <$> variable 
  , try $ constStr <$> predicateSymbol
  , try $ constInt <$> signedInteger
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
