module TypeInferenceSpec where

import Test.Hspec

import Swipl
import TestResults
import TypeInference

spec :: Spec
spec = parallel $ 
  do
    describe "TypeInference.typeInference" $ do
      inferPreserveSem "examples/prolog/market.pl"
      inferPreserveSem "examples/prolog/fib.pl"
      inferPreserveSem "examples/prolog/employee.pl"
      inferPreserveSem "examples/prolog/auction.pl"

inferPreserveSem :: String -> Spec
inferPreserveSem f =
  preservesSemantics typeInference f

