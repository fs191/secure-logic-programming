module PostProcessingSpec where

import Control.Monad

import Data.Maybe

import Test.Hspec

import Swipl
import PostProcessing
import Transform

spec :: Spec
spec =
  parallel . describe "PostProcessing.postProcess" $ do
    postProcPreserveSem "examples/prolog/market_xvars.pl"
    postProcPreserveSem "examples/prolog/market.pl"
    postProcPreserveSem "examples/prolog/auction.pl"
    postProcPreserveSem "examples/prolog/employee.pl"

postProcPreserveSem :: String -> Spec
postProcPreserveSem f = preservesSemantics (fromJust . _fun) f
  where _fun = (flip deriveAllGroundRules 3) >=> (return . postProcess)

