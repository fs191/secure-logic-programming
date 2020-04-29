module TransformSpec where

import Test.Hspec

import Control.Monad (void)

import Parser
import Transform

spec :: Spec
spec =
  describe "Transform.deriveAllGroundRules" $ do
    canDeriveOn "examples/ppdatalog/fib.pl" 1
    canDeriveOn "examples/ppdatalog/fib.pl" 3
    canDeriveOn "examples/ppdatalog/market.pl" 1
    canDeriveOn "examples/ppdatalog/market.pl" 3

canDeriveOn :: String -> Int -> Spec
canDeriveOn file n = it desc $
  do
    f <- parseDatalogFromFile file
    let d = deriveAllGroundRules f n
    return $ d `seq` ()
  where desc = "can derive ground rules on " ++ file ++ " with " ++ show n ++ " iterations"

