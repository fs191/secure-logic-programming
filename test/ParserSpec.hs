module ParserSpec where

import Test.Hspec

import Data.Either (isRight)

import Control.Exception (try, SomeException)

import Parser
import DatalogProgram

spec :: Spec
spec = describe "Parser" $ do
    canParse "examples/prolog/auction.pl"
    canParse "examples/prolog/fib.pl"
    canParse "examples/prolog/market.pl"
    canParse "examples/prolog/employee.pl"
    canParse "examples/prolog/relatives.pl"
    canParse "examples/prolog/test_script.pl"

canParse :: String -> Spec
canParse file =
  it
    ("can parse " ++ file)
    (action `shouldReturn` True)
  where action = do
              res <- try $ parseDatalogFromFile file :: IO (Either SomeException DatalogProgram)
              return $ isRight res
