module Parser.DatalogParser.Expr
  ( aExpr
  , aTerm
  , rule
  , list
  , attributeParse
  , predParse
  , varParse
  ) where

import Text.Megaparsec

import Data.Void (Void)
import Data.Foldable

import Parser.DatalogParser.Lexer
import Expr as E hiding (identifier)
import qualified Rule as R
import qualified Annotation as A

type Parser = Parsec Void String

-----------------------
-- Numeric expressions
-----------------------

aExpr :: Parser Expr
aExpr = aExpr1

aTerm :: Parser Expr
aTerm = asum
  [ try predParse
  , try varParse
  , try strParse
  , try intParse
  , try attributeParse
  , try list
  , typable $ parens aExpr
  ] <?> "term"

aExpr1 :: Parser Expr
aExpr1 = asum
  [ binary greaterEqual ">=" aExpr1 aExpr2
  , binary greater      ">"  aExpr1 aExpr2
  , binary equal        "="  aExpr1 aExpr2
  , binary lessEqual    "=<" aExpr1 aExpr2
  , binary less         "<"  aExpr1 aExpr2
  , binary eIs          "is" aExpr1 aExpr2
  , aExpr2
  ]

aExpr2 :: Parser Expr
aExpr2 = asum
  [ binary eAdd "+" aExpr2 aExpr3
  , binary eSub "-" aExpr2 aExpr3
  , aExpr3
  ]

aExpr3 :: Parser Expr
aExpr3 = asum
  [ binary eMul "*" aExpr3 aTerm
  , binary eDiv "/" aExpr3 aTerm
  , aTerm
  ]

list :: Parser Expr
list = eList <$> (brackets $ sepBy aTerm comma) <?> "list"

binary 
  :: (Expr -> Expr -> Expr) 
  -> String 
  -> Parser Expr 
  -> Parser Expr 
  -> Parser Expr
binary op sym p1 p2 = try . typable $ op <$> p2 <* symbol sym <*> p1

-----------------------
-- Helper functions
-----------------------

predParse :: Parser Expr
predParse = 
  do
    n <- identifier
    args <- parens $ sepBy1 aTerm comma
    typable . return $ predicate n args

intParse :: Parser Expr
intParse =
  do
    n <- signedInteger
    typable . return $ constInt n

varParse :: Parser Expr
varParse =
  do
    n <- variable
    typable . return $ var n

strParse :: Parser Expr
strParse = 
  do
    s <- identifier
    typable . return $ constStr s

rule :: Parser R.Rule
rule = 
  do
    psym <- identifier
    terms <- option [] . parens $ sepBy1 aTerm comma
    return $ R.fact psym terms

attributeParse :: Parser Expr
attributeParse =
  do
    s <- attributeIdentifier
    typable . return $ E.attribute s

typable :: Parser Expr -> Parser Expr
typable e =
  do
    e' <- e
    t <- try $ optional typing
    let f = case t of
          Just p  -> applyTyping p
          Nothing -> applyTyping A.emptyTyping
    return $ f e'
  <|> e

