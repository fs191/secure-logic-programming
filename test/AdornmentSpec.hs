module AdornmentSpec where

import Data.Either

import Test.Hspec

import Swipl
import Adornment
import TestResults
import DatalogProgram

spec :: Spec
spec = describe "Adornment.adornProgram" $ do
  adornPreserveSem "examples/prolog/market.pl"
  adornPreserveSem "examples/prolog/fib.pl"
  adornPreserveSem "examples/prolog/employee.pl"
  adornPreserveSem "examples/prolog/auction.pl"
  runsSuccessfully "examples/prolog/market.pl"        adornProgram marketRes
  runsSuccessfully "examples/prolog/employee.pl"      adornProgram employeeRes
  runsSuccessfully "examples/prolog/auction.pl"       adornProgram marketRes
  runsSuccessfullyDB "examples/ppdatalog/market.pl"   adornProgram marketRes marketDB
  runsSuccessfullyDB "examples/ppdatalog/employee.pl" adornProgram employeeRes employeeDB
  runsSuccessfullyDB "examples/ppdatalog/auction.pl"  adornProgram marketRes marketDB

adornPreserveSem :: String -> Spec
adornPreserveSem f = 
  preservesSemantics adornProgram f

