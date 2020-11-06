module ParserSpec where

import Relude

import Test.Hspec
import Test.HUnit

import Data.Either (isLeft)
import qualified Data.Text as T

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
    canParse "examples/prolog/ship.pl"
    canParse "examples/ppdatalog/fib.pl"
    canParse "examples/ppdatalog/market.pl"
    canParse "examples/ppdatalog/auction.pl"
    canParse "examples/ppdatalog/employee.pl"
    canParse "examples/ppdatalog/relatives.pl"
    canParse "examples/ppdatalog/fib_unfolded_3_fulltyped.pl"
    canParse "examples/ppdatalog/aggregation.pl"
    canParse "examples/ppdatalog/market_unfolded_fulltyped.pl"
    canParse "examples/ppdatalog/employee_unfolded_fulltyped.pl"
    canParse "examples/ppdatalog/relatives_unfolded_3_fulltyped.pl"
    canParse "examples/ppdatalog/ship.pl"
    canParse "examples/ppdatalog/ship_short.pl"
    canParse "examples/ppdatalog/precendence.pl"
    cannotParse "examples/prolog/negative/gibberish.pl"

canParse :: T.Text -> Spec
canParse file = it (toString $ "can parse " <> file) $ action `shouldReturn` ()
  where action = do
            res <- parseDatalog file <$> readFileText (toString file)
            case res of
              Right _ -> return ()
              Left  e -> error e

cannotParse :: T.Text -> Spec
cannotParse file = it (toString $ "does not parse " <> file) $ do
  res <- parseDatalog file <$> readFileText (toString file)
  assertBool "" $ isLeft res

