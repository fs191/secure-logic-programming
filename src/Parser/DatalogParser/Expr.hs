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

import Control.Lens

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

aTerm :: Parser Expr
aTerm = (withSrcPos $ asum
  [ try predParse
  , try varParse
  , try strParse
  , try intParse
  , try attributeParse
  , try list
  , typable $ parens aExpr
  ])
  <?> "term"

aExpr :: Parser Expr
aExpr = withSrcPos $ asum
  [ binary greaterEqual ">=" aExpr aExpr2
  , binary greater      ">"  aExpr aExpr2
  , binary equal        "="  aExpr aExpr2
  , binary lessEqual    "=<" aExpr aExpr2
  , binary less         "<"  aExpr aExpr2
  , binary eIs          "is" aExpr aExpr2
  , aExpr2
  ]

aExpr2 :: Parser Expr
aExpr2 = withSrcPos $ asum
  [ binary eAdd "+" aExpr2 aExpr3
  , binary eSub "-" aExpr2 aExpr3
  , aExpr3
  ]

aExpr3 :: Parser Expr
aExpr3 = withSrcPos $ asum
  [ binary eMul "*" aExpr3 aTerm
  , binary eDiv "/" aExpr3 aTerm
  , aTerm
  ]

list :: Parser Expr
list = (withSrcPos $ eList <$> (brackets $ sepBy aTerm comma)) <?> "list"

binary 
  :: (Expr -> Expr -> Expr) 
  -> String 
  -> Parser Expr 
  -> Parser Expr 
  -> Parser Expr
binary f sym p1 p2 = withSrcPos $ try . typable $ f <$> p2 <* symbol sym <*> p1

-----------------------
-- Helper functions
-----------------------

predParse :: Parser Expr
predParse = withSrcPos $
  do
    n <- identifier
    args <- parens $ sepBy1 aTerm comma
    typable . return $ predicate n args

intParse :: Parser Expr
intParse = withSrcPos $
  do
    n <- signedInteger
    typable . return $ constInt n

varParse :: Parser Expr
varParse = withSrcPos $
  do
    n <- variable
    typable . return $ var n

strParse :: Parser Expr
strParse = withSrcPos $
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
attributeParse = withSrcPos $
  do
    s <- attributeIdentifier
    typable . return $ E.attribute s

--
-- Useful combinators for expression parsing
--

typable :: Parser Expr -> Parser Expr
typable e =
  do
    e' <- e
    t <- try $ optional typing
    let f = case t of
          Just p  -> p
          Nothing -> A.emptyTyping
    return $ applyTyping f e'
  <|> e

withSrcPos :: Parser Expr -> Parser Expr
withSrcPos parser =
  do
    begin <- getSourcePos
    res <- parser
    end <- getSourcePos
    return $ res & annotation . A.srcPos .~ Just (begin, end)

