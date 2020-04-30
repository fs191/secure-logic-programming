
module DatalogProgramSpec where

import System.Timeout
import System.IO.Unsafe

import Test.Hspec

import Parser.DatalogParser

spec :: Spec
spec = describe "DatalogProgramSpec.show" $ do
    canShow "examples/prolog/auction.pl"
    canShow "examples/prolog/fib.pl"
    canShow "examples/prolog/market.pl"
    canShow "examples/prolog/employee.pl"
    canShow "examples/prolog/relatives.pl"
    canShow "examples/ppdatalog/fib.pl"
    canShow "examples/ppdatalog/market.pl"
    canShow "examples/ppdatalog/auction.pl"
    canShow "examples/ppdatalog/employee.pl"
    canShow "examples/ppdatalog/relatives.pl"

canShow :: String -> Spec
canShow file = it "can show DatalogProgram within 3 seconds" $
  action `shouldBe` Just ""
  where action = unsafePerformIO . timeout 3000 $ do
          res <- parseDatalogFromFile file
          return $ show res

