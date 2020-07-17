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
  runsSuccessfully "examples/prolog/market.pl"        adorn marketRes
  runsSuccessfully "examples/prolog/employee.pl"      adorn employeeRes
  runsSuccessfully "examples/prolog/auction.pl"       adorn marketRes
  runsSuccessfullyDB "examples/ppdatalog/market.pl"   adorn marketRes marketDB
  runsSuccessfullyDB "examples/ppdatalog/employee.pl" adorn employeeRes employeeDB
  runsSuccessfullyDB "examples/ppdatalog/auction.pl"  adorn marketRes marketDB

adorn :: DatalogProgram -> DatalogProgram
adorn = fromRight undefined . adornProgram

adornPreserveSem :: String -> Spec
adornPreserveSem f = 
  preservesSemantics (fromRight (error "adornment failed") . adornProgram) f

