{-# LANGUAGE OverloadedStrings #-}

module TransformSpec where

import Test.Hspec

import Data.Text.Prettyprint.Doc
import Data.List

import Parser.DatalogParser
import Transform
import Rule
import DatalogProgram
import Expr

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
    transContains "examples/prolog/broken.pl" 3 
      [ rule "h" 
        [constStr "a", constStr "c", constStr "e"] 
        (constTrue)
      ]

canDeriveOn :: String -> Int -> Spec
canDeriveOn file n = it desc $ action `shouldReturn` ()
  where 
    desc = "can derive ground rules on " ++ file ++ " with " ++ show n ++ " iterations"
    action = do
        f <- parseDatalogFromFile file
        let d = deriveAllGroundRules f n
        return $ d `seq` ()

transContains :: String -> Int -> [Rule] -> Spec
transContains file n rs = it desc $
  do
    f <- parseDatalogFromFile file
    let d = deriveAllGroundRules f n
    if rules d `intersect` rs == rs
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
     
