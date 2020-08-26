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
import Control.Monad

import Data.Void (Void)
import Data.Foldable

import Parser.DatalogParser.Lexer
import Expr as E hiding (identifier)
import qualified Rule as R
import qualified Annotation as A

import Debug.Trace

type Parser = Parsec Void String

-----------------------
-- Numeric expressions
-----------------------

aTerm :: Parser Expr
aTerm = (withSrcPos $ asum
  [ try predParse
  , try varParse
  , try strParse
  , try floatParse
  , try intParse
  , try attributeParse
  , try holeParse
  , try list
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
  [ binary eMul "*" aExpr3 aExpr4
  , binary eDiv "/" aExpr3 aExpr4
  , aExpr4
  ]

aExpr4 :: Parser Expr
aExpr4 = withSrcPos $ asum
  [ binary ePow "^" aExpr4 aTerm
  , try sqrtParse
  , try aTerm
  , typable $ parens aExpr
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
    args <- parens $ sepBy aTerm comma
    typable . return $ predicate n args

sqrtParse :: Parser Expr
sqrtParse = withSrcPos $
  do
    void $ symbol "sqrt"
    x <- parens aExpr
    return $ eSqrt x

intParse :: Parser Expr
intParse = withSrcPos $
  do
    n <- signedInteger
    typable . return $ constInt n

floatParse :: Parser Expr
floatParse = withSrcPos $
  do
    x <- signedFloat
    typable . return $ constFloat x

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

holeParse :: Parser Expr
holeParse =
  do
    void $ symbol "_"
    void $ optional variable
    typable . return $ hole

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
    t <- optional $ try typing
    let f = case t of
          Just p  -> p
          Nothing -> A.empty
    return $ applyAnnotation f e'

withSrcPos :: Parser Expr -> Parser Expr
withSrcPos parser =
  do
    begin <- getSourcePos
    res <- parser
    end <- getSourcePos
    return $ res & annotation . A.srcPos .~ Just (begin, end)

