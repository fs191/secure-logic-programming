module Parser.DatalogParser.Expr
  ( aExpr, bPredExpr
  , term, predicateSymbol
  , literal
  ) where

import Text.Megaparsec

import Control.Monad.Combinators.Expr

import Data.Void (Void)

import Parser.DatalogParser.Lexer
import Aexpr
import Rule

type Parser = Parsec Void String

-----------------------
-- Predicate expressions
-----------------------

bPredExpr :: Parser (BExpr DBVar)
bPredExpr = 
      opParse BLE "=<"
  <|> opParse BLT "<"
  <|> opParse BGE ">="
  <|> opParse BGT ">"
  <|> opParse BEQ "="
  <|> opParse BEQ "is"
  <|> (try $ factToPred <$> literal)

-----------------------
-- Numeric expressions
-----------------------

aExprTable :: [[Operator Parser (AExpr DBVar)]]
aExprTable = 
  [ [ prefix "-" $ aUn ANeg
    ]
  , [ binary "\\/" $ aBin AMax
    , binary "/\\" $ aBin AMin
    ]
  , [ binary "*" $ aBin AMult
    , binary "/" $ aBin ADiv
    ]
  , [ binary "+" $ aBin AAdd
    , binary "-" $ aBin ASub
    ]
  ]

aExpr :: Parser (AExpr DBVar)
aExpr = makeExprParser aTerm aExprTable

aTerm :: Parser (AExpr DBVar)
aTerm = term <|> parens aExpr

term :: Parser (AExpr DBVar)
term = 
      (try $ aVar      <$> variable )
  <|> (try $ aStrLit   <$> predicateSymbol)
  <|> (try $ aNumLit   <$> signedInteger)
  <|> (fail $ "could not parse term")

-----------------------
-- Helper functions
-----------------------

binary :: String -> (a -> a -> a) -> Operator Parser a
binary n f = InfixL  (f <$ symbol n)

prefix :: String -> (a -> a) -> Operator Parser a
prefix n f = Prefix  (f <$ symbol n)

literal :: Parser Fact
literal = 
  do
    psym <- predicateSymbol
    terms <- option [] . parens $ sepBy1 term comma
    let terms' = terms
    return $ fact psym terms'

opParse :: BBinPredOp -> String -> Parser (BExpr DBVar)
opParse op s = 
  do
    expr <- try $ bBinPred op <$> aExpr <*> (symbol s *> aExpr)
    return $ expr

factToPred :: Fact -> BExpr DBVar
factToPred f = bPred (functor f) (args f)

