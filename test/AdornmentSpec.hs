module AdornmentSpec where

import Data.Either

import Test.Hspec

import Swipl
import Adornment

spec :: Spec
spec = describe "Adornment.adornProgram" $ do
  adornPreserveSem "examples/prolog/market.pl"
  adornPreserveSem "examples/prolog/fib.pl"
  adornPreserveSem "examples/prolog/employee.pl"
  adornPreserveSem "examples/prolog/auction.pl"

adornPreserveSem :: String -> Spec
adornPreserveSem f = 
  preservesSemantics (fromRight (error "adornment failed") . adornProgram) f
