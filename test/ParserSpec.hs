module ParserSpec where

import Test.Hspec
import Test.HUnit

import Data.Either (isLeft)

import Text.Megaparsec.Error (errorBundlePretty)

import Parser.DatalogParser

spec :: Spec
spec = parallel . describe "Parser.parseDatalog" $ do
    canParse "examples/prolog/onlyFacts.pl"
    canParse "examples/prolog/onlyRules.pl"
    canParse "examples/prolog/onlyGoal.pl"
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
    canParse "examples/ppdatalog/fib_unfolded_3_fulltyped.pl"
    canParse "examples/ppdatalog/market_unfolded_fulltyped.pl"
    canParse "examples/ppdatalog/employee_unfolded_fulltyped.pl"
    canParse "examples/ppdatalog/relatives_unfolded_3_fulltyped.pl"
    canParse "examples/ppdatalog/ship.pl"
    cannotParse "examples/prolog/negative/gibberish.pl"

canParse :: String -> Spec
canParse file = it ("can parse " ++ file) $ action `shouldReturn` ()
  where action = do
            res <- parseDatalog file <$> readFile file
            case res of
              Right _ -> return ()
              Left  e -> error $ errorBundlePretty e

cannotParse :: String -> Spec
cannotParse file = it ("does not parse " ++ file) $ do
  res <- parseDatalog file <$> readFile file
  assertBool "" $ isLeft res

