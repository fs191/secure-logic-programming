module Parser.DatalogParser.Expr
  ( aExpr
  , term
  , rule
  , attributeParse
  , predParse
  ) where

import Text.Megaparsec

import Control.Lens
import Control.Monad.Combinators.Expr

import Data.Void (Void)
import Data.Foldable

import Parser.DatalogParser.Lexer
import Expr as E hiding (identifier)
import qualified Rule as R

type Parser = Parsec Void String

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
  , [ binary "=<" lessEqual
    , binary "<"  less
    , binary ">=" greaterEqual
    , binary ">"  greater
    , binary "="  equal
    , binary "is" eIs
    ]
  ]

aExpr :: Parser Expr
aExpr = typable $ makeExprParser aTerm aExprTable

aTerm :: Parser Expr
aTerm = try term

term :: Parser Expr
term = asum
  [ try predParse
  , try varParse
  , try strParse
  , try intParse
  , try attributeParse
  , try list
  , parens aExpr
  ] <?> "term"

list :: Parser Expr
list = eList <$> (brackets $ sepBy term comma) <?> "list"

-----------------------
-- Helper functions
-----------------------

binary :: String -> (a -> a -> a) -> Operator Parser a
binary n f = InfixL  (f <$ symbol n)

prefix :: String -> (a -> a) -> Operator Parser a
prefix n f = Prefix  (f <$ symbol n)

predParse :: Parser Expr
predParse = 
  do
    n <- identifier
    args <- parens $ sepBy1 term comma
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
    terms <- option [] . parens $ sepBy1 term comma
    return $ R.fact psym terms

typeExpr :: (PPDomain, PPType) -> Expr -> Expr
typeExpr (dom, dat) e = e & annLens . domain  .~ dom
                       & annLens . annType .~ dat

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
          Just p  -> typeExpr p
          Nothing -> typeExpr (Public, PPStr)
    return $ f e'
  <|> e

