module PostProcessingSpec where

import Relude

import Test.Hspec

import Control.Monad

import Swipl
import PostProcessing
import Transform
import Expr
import TestResults

spec :: Spec
spec =
  parallel . describe "PostProcessing.postProcess" $ do
    postProcPreserveSem "examples/ppdatalog/market.pl"   marketDB
    postProcPreserveSem "examples/ppdatalog/auction.pl"  marketDB
    postProcPreserveSem "examples/ppdatalog/employee.pl" employeeDB

postProcPreserveSem :: Text -> [Expr] -> Spec
postProcPreserveSem f db = preservesSemanticsDB _fun f db
  where _fun = return . postProcess <=< deriveAllGroundRules 3

