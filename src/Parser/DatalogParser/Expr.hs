module Parser.DatalogParser.Expr
  ( aExpr, bPredExpr
  , term, predicateSymbol
  ) where

import Text.Megaparsec

import Control.Monad.Combinators.Expr

import Data.Void (Void)

import Parser.DatalogParser.Lexer
import Aexpr

type Parser = Parsec Void String

-----------------------
-- Predicate expressions
-----------------------

bPredExpr :: Parser (BExpr String)
bPredExpr = 
      opParse BLE "=<"
  <|> opParse BLT "<"
  <|> opParse BGE ">="
  <|> opParse BGT ">"
  <|> opParse BEQ "="
  <|> opParse BEQ "is"
  where

-----------------------
-- Numeric expressions
-----------------------

aExprTable :: [[Operator Parser (AExpr String)]]
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

aExpr :: Parser (AExpr String)
aExpr = makeExprParser aTerm aExprTable

aTerm :: Parser (AExpr String)
aTerm = term <|> parens aExpr

term :: Parser (AExpr String)
term = 
      (try $ aVar <$> variable )
  <|> (try $ aStrLit <$> predicateSymbol)
  <|> (try $ aNumLit <$> signedInteger)

-----------------------
-- Helper functions
-----------------------

binary :: String -> (a -> a -> a) -> Operator Parser a
binary  name f = InfixL  (f <$ symbol name)

prefix :: String -> (a -> a) -> Operator Parser a
prefix  name f = Prefix  (f <$ symbol name)

opParse :: BBinPredOp -> String -> Parser (BExpr String)
opParse op s = try $ bBinPred op <$> aExpr <*> (symbol s *> aExpr)

