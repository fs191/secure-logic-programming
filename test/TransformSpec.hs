module TransformSpec where

import Test.Hspec

import Parser.DatalogParser
import Transform

spec :: Spec
spec =
  describe "Transform.deriveAllGroundRules" $ do
    canDeriveOn "examples/ppdatalog/fib.pl" 1
    canDeriveOn "examples/ppdatalog/fib.pl" 3
    canDeriveOn "examples/ppdatalog/market.pl" 1
    canDeriveOn "examples/ppdatalog/market.pl" 3
    canDeriveOn "examples/ppdatalog/auction.pl" 1
    canDeriveOn "examples/ppdatalog/auction.pl" 3
    canDeriveOn "examples/ppdatalog/employee.pl" 1
    canDeriveOn "examples/ppdatalog/employee.pl" 3
    canDeriveOn "examples/ppdatalog/relatives.pl" 1
    canDeriveOn "examples/ppdatalog/relatives.pl" 3

canDeriveOn :: String -> Int -> Spec
canDeriveOn file n = it desc $ action `shouldReturn` ()
  where 
    desc = "can derive ground rules on " ++ file ++ " with " ++ show n ++ " iterations"
    action = do
        f <- parseDatalogFromFile file
        let d = deriveAllGroundRules f n
        return $ d `seq` ()

