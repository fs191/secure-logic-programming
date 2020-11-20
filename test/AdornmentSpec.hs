module AdornmentSpec where

import Relude

import Data.Either

import Test.Hspec

import Translator.Adornment

import Swipl
import TestResults
import DatalogProgram

spec :: Spec
spec = describe "Adornment.adornProgram" $ do
  adornPreserveSem "examples/prolog/market.pl"
  adornPreserveSem "examples/prolog/fib.pl"
  adornPreserveSem "examples/prolog/employee.pl"
  adornPreserveSem "examples/prolog/auction.pl"
  --adornPreserveSem "examples/ppdatalog/ship_new.pl"
  adornPreserveSem "examples/ppdatalog/precendence.pl"
  runsSuccessfully "examples/prolog/market.pl"        (return . adornProgram) marketRes
  runsSuccessfully "examples/prolog/employee.pl"      (return . adornProgram) employeeRes
  runsSuccessfully "examples/prolog/auction.pl"       (return . adornProgram) marketRes
  runsSuccessfullyDB "examples/ppdatalog/market.pl"   (return . adornProgram) marketRes marketDB
  runsSuccessfullyDB "examples/ppdatalog/employee.pl" (return . adornProgram) employeeRes employeeDB
  runsSuccessfullyDB "examples/ppdatalog/auction.pl"  (return . adornProgram) marketRes marketDB

adornPreserveSem :: Text -> Spec
adornPreserveSem f = 
  preservesSemantics (return . adornProgram) f

