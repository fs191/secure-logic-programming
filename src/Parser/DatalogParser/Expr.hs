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

type Parser = Parsec Void String
data Operator f = Operator String f

-------------------------
-- Numeric expressions --
-------------------------

aTerm :: Parser Expr
aTerm = (withSrcPos $ asum
  [ varParse
  , predParse
  , strParse
  , attributeParse
  , try floatParse
  , intParse
  , list
  ])
  <?> "term"

aExpr :: Parser Expr
aExpr = binary ops aExpr aExpr1
  where
    ops =
      [ Operator ";" eOr
      ]

aExpr1 :: Parser Expr
aExpr1 = binary ops aExpr1 aExpr2
  where
    ops =
      [ Operator ">="  greaterEqual
      , Operator "=<"  lessEqual
      , Operator ">"   greater
      , Operator "<"   less
      , Operator "=:=" equal
      , Operator "="   equal
      , Operator "is"  eIs
      ]

aExpr2 :: Parser Expr
aExpr2 = binary ops aExpr2 aExpr3
  where
    ops =
      [ Operator "+" eAdd
      , Operator "-" eSub
      ]

aExpr3 :: Parser Expr
aExpr3 = binary ops aExpr3 aExpr4
  where 
    ops =
      [ Operator "*" eMul
      , Operator "/" eDiv
      ]

aExpr4 :: Parser Expr
aExpr4 = binary ops aExpr4 aExpr5
  where
    ops = 
      [ Operator "^" ePow
      ]

aExpr5 :: Parser Expr
aExpr5 = unary ops aExpr <|> aTerm <|> par
  where
    par = typable . lexeme $ parens aExpr
    ops =
      [ Operator "sqrt" eSqrt
      , Operator "\\+"  eNot
      ]

unary 
  :: [Operator (Expr -> Expr)] 
  -> Parser Expr 
  -> Parser Expr
unary ops next = typable $
  do
    f <- try $ do
      f <- choice $ try . parseOperator <$> ops
      void $ symbol "("
      return f
    x <- lexeme next
    void $ symbol ")"
    return $ f x

binary 
  :: [Operator (Expr -> Expr -> Expr)] 
  -> Parser Expr 
  -> Parser Expr 
  -> Parser Expr
binary ops this next = (typable $
  do
    f <- try $ do
      lhs <- next
      opr <- choice $ try . parseOperator <$> ops
      return $ opr lhs
    rhs <- this
    return $ f rhs
  ) <|> next

parseOperator :: Operator f -> Parser f
parseOperator (Operator sym f) = symbol sym *> return f

list :: Parser Expr
list = (withSrcPos $ eList <$> (brackets $ sepBy aTerm comma)) <?> "list"

-----------------------
-- Helper functions
-----------------------

predParse :: Parser Expr
predParse = withSrcPos $
  do
    n <- try $ do
      n <- identifier
      void $ symbol "("
      return n
    args <- sepBy aTerm comma
    void $ symbol ")"
    typable . return $ predicate n args

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
    typable . return $ case n of
      "_" -> hole
      _   -> var n

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
