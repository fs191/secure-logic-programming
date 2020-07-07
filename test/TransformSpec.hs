{-# LANGUAGE OverloadedStrings #-}

module TransformSpec where

import Test.Hspec

import Data.Text

import Control.Monad.IO.Class

import Parser.DatalogParser

import Transform
import Swipl
import Expr
import DatalogProgram
import TestResults

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
    transPreserveSem "examples/prolog/relatives.pl" 1
    transPreserveSemDB "examples/ppdatalog/market.pl" 3 marketDB
    transPreserveSemDB "examples/ppdatalog/auction.pl" 3 marketDB
    transPreserveSemDB "examples/ppdatalog/employee.pl" 3 employeeDB
    runsSuccessfully "examples/prolog/fib.pl" transform ["[2]"]
    runsSuccessfully "examples/prolog/market.pl" transform marketRes
    runsSuccessfully "examples/prolog/auction.pl" transform marketRes
    runsSuccessfully "examples/prolog/employee.pl" transform employeeRes
    runsSuccessfullyDB "examples/ppdatalog/market.pl" transform marketRes marketDB
    runsSuccessfullyDB "examples/ppdatalog/auction.pl" transform marketRes marketDB
    runsSuccessfullyDB "examples/ppdatalog/employee.pl" transform employeeRes employeeDB
    
transform :: DatalogProgram -> DatalogProgram
transform = flip deriveAllGroundRules 2

canDeriveOn :: String -> Int -> Spec
canDeriveOn file n = it desc $ action `shouldReturn` ()
  where 
    desc = "can derive ground rules on " ++ file ++ " with " ++ show n ++ " iterations"
    action = do
        f <- parseDatalogFromFile file
        let d = deriveAllGroundRules f n
        return $ d `seq` ()
     
transPreserveSem :: String -> Int -> Spec
transPreserveSem f n = 
  preservesSemantics (flip deriveAllGroundRules n) f

transPreserveSemDB :: String -> Int -> [Expr] -> Spec
transPreserveSemDB f n db = 
  preservesSemanticsDB (flip deriveAllGroundRules n) f db

--
-- Expected results
--

