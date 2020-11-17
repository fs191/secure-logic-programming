module PostProcessingSpec where

import Relude

import Test.Hspec

import Control.Exception
import Control.Monad
import Control.Monad.Except

import Adornment
import TypeInference
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
postProcPreserveSem f db = 
  do
    preservesSemanticsDB _fun f db
  where 
    _fun x = either throw id <$> act x
    act x = runExceptT $
      do
        tr <- liftIO $ deriveAllGroundRules 3 x
        postProcess tr
