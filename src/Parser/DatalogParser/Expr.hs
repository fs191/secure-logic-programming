module Parser.DatalogParser.Expr
  ( aExpr
  , aTerm
  , rule
  , list
  , predParse
  , varParse
  , aggregation
  ) where

import Relude

import Text.Megaparsec

import Control.Lens

import Parser.DatalogParser.Lexer
import Expr as E hiding (identifier)
import qualified Rule as R
import qualified Annotation as A
import ErrorMsg

type Parser = Parsec CompilerException Text
data Operator f = Operator Text f

-------------------------
-- Numeric expressions --
-------------------------

aTerm :: Parser Expr
aTerm = (withSrcPos $ asum
  [ varParse
  , predParse
  , boolParse
  , strParse
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
      , Operator "=/=" eNeq
      , Operator "="   eUn
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
aExpr3 = binaryFuns funs aExpr <|> binary ops aExpr3 aExpr4
  where 
    ops =
      [ Operator "*" eMul
      , Operator "/" eFDiv
      ]
    funs =
      [ Operator "mod" eMod
      ]

aExpr4 :: Parser Expr
aExpr4 = binary ops aExpr4 aExpr5
  where
    ops = 
      -- TODO exponentiation should be evaluated from right to left
      [ Operator "^" ePow
      ]

aExpr5 :: Parser Expr
aExpr5 = unary ops aExpr <|> aTerm <|> par
  where
    par = typable $ parens aExpr
    ops =
      [ Operator "sqrt" eSqrt
      , Operator "\\+"  eNot
      ]

unary 
  :: [Operator (Expr -> Expr)] 
  -> Parser Expr 
  -> Parser Expr
unary ops next = typable . withSrcPos $
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
      opr <- choice $ parseOperator <$> ops
      return $ opr lhs
    rhs <- this
    return $ f rhs
  ) <|> next

binaryFuns
  :: [Operator (Expr -> Expr -> Expr)] 
  -> Parser Expr 
  -> Parser Expr 
binaryFuns funs next = (typable $ 
  do
    f <- choice $ parseOperator <$> funs
    void $ symbol "("
    x <- next
    void $ symbol ","
    y <- next
    void $ symbol ")"
    return $ f x y
  )

parseOperator :: Operator f -> Parser f
parseOperator (Operator sym f) = symbol sym *> return f

list :: Parser Expr
list = (withSrcPos $ eList <$> (brackets $ sepBy aTerm comma)) <?> "list"

-----------------------
-- Helper functions
-----------------------

aggregation :: Parser Expr
aggregation = choice $ f <$> [minBound..maxBound]
  where
    f ag =
      do
        try $ do
          void . symbol $ show ag
          void $ symbol "("
        p <- predParse
        void $ symbol ","
        v <- varParse
        void $ symbol ","
        o <- varParse
        void $ symbol ")"
        return $ eAggr ag p v o

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

boolParse :: Parser Expr
boolParse = choice
  [ symbol "true"  *> return constTrue
  , symbol "false" *> return constFalse
  ]

--
-- Useful combinators for expression parsing
--

typable :: Parser Expr -> Parser Expr
typable e =
  do
    e' <- e
    t <- optional typing
    let f = case t of
          Just p  -> p
          Nothing -> A.empty
    let err = customFailure $ TypeApplicationFailed (f ^. annType) e'
    maybe err return $ applyAnnotation f e'

withSrcPos :: Parser Expr -> Parser Expr
withSrcPos parser =
  do
    begin <- getSourcePos
    res <- parser
    return $ res & annotation . A.srcPos .~ Just begin
