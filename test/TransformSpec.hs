{-# LANGUAGE OverloadedStrings #-}

module TransformSpec where

import Test.Hspec

import Parser.DatalogParser
import Transform
import Swipl

spec :: Spec
spec =
  parallel . describe "Transform.deriveAllGroundRules" $ do
    transPreserveSem "examples/prolog/fib.pl" 1
    transPreserveSem "examples/prolog/fib.pl" 3
    transPreserveSem "examples/prolog/market.pl" 1
    transPreserveSem "examples/prolog/market.pl" 3
    transPreserveSem "examples/prolog/auction.pl" 1
    transPreserveSem "examples/prolog/auction.pl" 3
    transPreserveSem "examples/prolog/employee.pl" 1
    transPreserveSem "examples/prolog/employee.pl" 3
    transPreserveSem "examples/prolog/relatives.pl" 1
    runsSuccessfully "examples/prolog/fib.pl" ["2"]
    runsSuccessfully "examples/prolog/market.pl"  ["alice"]
    runsSuccessfully "examples/prolog/auction.pl" ["wololo"]
    runsSuccessfully "examples/prolog/employee.pl"  ["wololo"]
    runsSuccessfully "examples/prolog/relatives.pl"  ["wololo"]
    

canDeriveOn :: String -> Int -> Spec
canDeriveOn file n = it desc $ action `shouldReturn` ()
  where 
    desc = "can derive ground rules on " ++ file ++ " with " ++ show n ++ " iterations"
    action = do
        f <- parseDatalogFromFile file
        let d = deriveAllGroundRules f n
        return $ d `seq` ()
     
transPreserveSem :: String -> Int -> Spec
transPreserveSem f n = preservesSemantics (flip deriveAllGroundRules n) f

