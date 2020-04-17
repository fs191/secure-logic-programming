module ParserSpec where

import Test.Hspec
import Test.HUnit

import Data.Either (isRight, isLeft, fromRight)

import Control.Exception (try, SomeException)

import Parser
import DatalogProgram

spec :: Spec
spec = describe "Parser.parseDatalogFromFile" $ do
    canParse "examples/prolog/auction.pl"
    canParse "examples/prolog/fib.pl"
    canParse "examples/prolog/market.pl"
    canParse "examples/prolog/employee.pl"
    canParse "examples/prolog/relatives.pl"
    canParse "examples/prolog/test_script.pl"
    cannotParse "examples/prolog/negative/gibberish.pl"

canParse :: String -> Spec
canParse file = it ("can parse " ++ file) $ do
  res <- parseDatalogFromFile file
  return ()

cannotParse :: String -> Spec
cannotParse file = it ("does not parse " ++ file) $ do
  res <- parseDatalog file <$> readFile file
  shouldSatisfy res isLeft
