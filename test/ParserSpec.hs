module ParserSpec where

import Test.Hspec

import Data.Either (isRight)

import Control.Exception (try, SomeException)

import Parser
import DatalogProgram

spec :: IO ()
spec = hspec $ do
  describe "Parser" $ do
    canParse "examples/prolog/auction.pl"
    canParse "examples/prolog/fib.pl"

canParse file =
  it
    ("can parse " ++ file)
    (action `shouldReturn` True)
  where action = do
        res <- try $ parseDatalogFromFile file :: IO (Either SomeException DatalogProgram)
        return $ isRight res
