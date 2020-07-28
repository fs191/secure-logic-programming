module TypeInferenceSpec where

import Test.Hspec

import Control.Lens

import Data.Generics.Uniplate.Data as U
import Data.Text.Prettyprint.Doc

import Adornment
import Annotation
import Expr
import Rule
import Swipl
import TypeInference
import DatalogProgram
import TestResults

import Parser.DatalogParser

newtype StringWrapper = StringWrapper String
  deriving(Eq)

instance Show StringWrapper where
  show (StringWrapper x) = x <> "\n\n"

wrap :: DatalogProgram -> StringWrapper
wrap = StringWrapper . show . pretty

spec :: Spec
spec = parallel $ 
  do
    describe "TypeInference.typeInference" $ do
      inferPreserveSemDB "examples/ppdatalog/market_unfolded_fulltyped.pl" marketDB
      --inferPreserveSemDB "examples/ppdatalog/fib_unfolded_3_fulltyped.pl" []
      inferPreserveSemDB "examples/ppdatalog/employee_unfolded_fulltyped.pl" employeeDB
      inferPreserveSemDB "examples/ppdatalog/relatives_unfolded_3_fulltyped.pl" relativesDB
      infersTypes "examples/ppdatalog/market_unfolded_fulltyped.pl"
      infersTypes "examples/ppdatalog/fib_unfolded_3_fulltyped.pl"
      infersTypes "examples/ppdatalog/employee_unfolded_fulltyped.pl"
      infersTypes "examples/ppdatalog/relatives_unfolded_3_fulltyped.pl"

inferPreserveSemDB :: String -> [Expr] -> Spec
inferPreserveSemDB f db =
  preservesSemanticsDB (typeInference . adornProgram) f db

infersTypes :: String -> Spec
infersTypes n = it desc $
  do
    f <- adornProgram <$> parseDatalogFromFile n
    let g = f & dpRules . traversed . ruleHead %~ clearTypings
              & dpRules . traversed . ruleTail %~ clearTypings
              & outputs %~ clearTypings
              & dpGoal %~ clearTypings
    (wrap $ typeInference g) `shouldBe` 
      (wrap f)
  where desc = "infers types properly for " <> n

clearTypings :: Expr -> Expr
clearTypings e = U.transform f e
  where
    f = annotation . typing .~ emptyTyping

