module PostProcessingSpec where

import Relude

import Test.Hspec

import Control.Exception
import Control.Monad
import Control.Monad.Except

import Translator.Adornment
import Translator.PreProcessing
import Translator.PostProcessing
import Translator.PKTransform
import Translator.Transform

import Swipl
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
        pp <- preProcess $ adornProgram x
        tr <- liftIO $ deriveAllGroundRules 10 pp
        postProcess $ pkTransform tr
