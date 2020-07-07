module PostProcessingSpec where

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
postProcPreserveSem f = preservesSemantics _fun f
  where _fun = postProcess . flip deriveAllGroundRules 3

