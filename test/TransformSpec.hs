{-# LANGUAGE OverloadedStrings #-}

module TransformSpec where

import Test.Hspec

import Data.Text
import Parser.DatalogParser
import Transform
import Swipl

marketRes :: [Text]
marketRes = 
  [ "[eve]" 
  , "[dave]"
  ]

employeeRes :: [Text]
employeeRes = 
  [ "[bob,0]"
  , "[alice,2000]"
  , "[henry,1200]"
  , "[peggy,2100]"
  , "[victor,1900]"
  ]

spec :: Spec
spec =
  parallel . describe "Transform.deriveAllGroundRules" $ do
    transPreserveSem "examples/prolog/fib.pl" 1
    transPreserveSem "examples/prolog/market_xvars.pl" 1
    transPreserveSem "examples/prolog/market.pl" 1
    transPreserveSem "examples/prolog/market.pl" 2
    transPreserveSem "examples/prolog/auction.pl" 1
    transPreserveSem "examples/prolog/auction.pl" 2
    transPreserveSem "examples/prolog/employee.pl" 1
    transPreserveSem "examples/prolog/employee.pl" 2
    --transPreserveSem "examples/prolog/relatives.pl" 1
    runsSuccessfully "examples/prolog/fib.pl" ["[2]"]
    runsSuccessfully "examples/prolog/market.pl"  marketRes
    runsSuccessfully "examples/prolog/auction.pl" marketRes
    runsSuccessfully "examples/prolog/employee.pl"  employeeRes
    

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

