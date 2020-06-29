{-# LANGUAGE OverloadedStrings #-}

module TransformSpec where

import Test.Hspec

import Data.Text.Prettyprint.Doc
import Data.List

import Parser.DatalogParser
import Transform
import Rule
import DatalogProgram
import Swipl

spec :: Spec
spec =
  describe "Transform.deriveAllGroundRules" $ do
    transPreserveSem "examples/prolog/fib.pl" 1
    transPreserveSem "examples/prolog/fib.pl" 3
    transPreserveSem "examples/prolog/market.pl" 1
    transPreserveSem "examples/prolog/market.pl" 3
    transPreserveSem "examples/prolog/auction.pl" 1
    transPreserveSem "examples/prolog/auction.pl" 3
    transPreserveSem "examples/prolog/employee.pl" 1
    transPreserveSem "examples/prolog/employee.pl" 3
    transPreserveSem "examples/prolog/relatives.pl" 1

canDeriveOn :: String -> Int -> Spec
canDeriveOn file n = it desc $ action `shouldReturn` ()
  where 
    desc = "can derive ground rules on " ++ file ++ " with " ++ show n ++ " iterations"
    action = do
        f <- parseDatalogFromFile file
        let d = deriveAllGroundRules f n
        return $ d `seq` ()

transContainsOnly :: String -> Int -> [Rule] -> Spec
transContainsOnly = transContains' const

transContains :: String -> Int -> [Rule] -> Spec
transContains = transContains' intersect

transContains' :: ([Rule] -> [Rule] -> [Rule]) -> String -> Int -> [Rule] -> Spec
transContains' fun file n rs = it desc $
  do
    f <- parseDatalogFromFile file
    let d = deriveAllGroundRules f n
    if fun (rules d) rs == rs
      then return ()
      else expectationFailure . failMsg $ rules d
  where
    desc = show $ hsep
      [ "Transformation of" 
      , pretty file 
      , "with"
      , pretty n 
      , "iterations contains"
      , hardline
      , indent 6 $ list $ pretty <$> rs
      ]
    failMsg d = show $ vsep
      [ vsep $ pretty <$> d
      , softline
      , "does not contain"
      , softline
      , vsep $ pretty <$> rs
      ]
     
transPreserveSem :: String -> Int -> Spec
transPreserveSem f n = preservesSemantics (flip deriveAllGroundRules n) f

