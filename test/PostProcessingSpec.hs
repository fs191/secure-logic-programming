module PostProcessingSpec where

import Control.Monad

import Data.Maybe

import Test.Hspec

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

postProcPreserveSem :: String -> [Expr] -> Spec
postProcPreserveSem f db = preservesSemanticsDB (fromJust . _fun) f db
  where _fun = (flip deriveAllGroundRules 3) >=> (return . postProcess)

