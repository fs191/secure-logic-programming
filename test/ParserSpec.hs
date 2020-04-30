module ParserSpec where

import Test.Hspec
import Test.Hspec.Expectations
import Test.HUnit

import Data.Either (isRight, isLeft, fromRight, fromLeft)

import Control.Exception (try, SomeException)
import Control.Monad (void)

import Text.Megaparsec.Error (errorBundlePretty)

import Debug.Trace

import Parser.DatalogParser
import DatalogProgram

spec :: Spec
spec = describe "Parser.parseDatalog" $ do
    canParse "examples/prolog/auction.pl"
    canParse "examples/prolog/fib.pl"
    canParse "examples/prolog/market.pl"
    canParse "examples/prolog/employee.pl"
    canParse "examples/prolog/relatives.pl"
    canParse "examples/ppdatalog/fib.pl"
    canParse "examples/ppdatalog/market.pl"
    canParse "examples/ppdatalog/auction.pl"
    canParse "examples/ppdatalog/employee.pl"
    canParse "examples/ppdatalog/relatives.pl"
    cannotParse "examples/prolog/negative/gibberish.pl"

canParse :: String -> Spec
canParse file = it ("can parse " ++ file) $ action `shouldReturn` Nothing
  where action = do
            res <- parseDatalog file <$> readFile file
            case res of
              Right x -> return Nothing
              Left  e -> return . Just $ errorBundlePretty e

cannotParse :: String -> Spec
cannotParse file = it ("does not parse " ++ file) $ do
  res <- parseDatalog file <$> readFile file
  assertBool "" $ isLeft res

