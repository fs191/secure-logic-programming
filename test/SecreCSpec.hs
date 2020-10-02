module SecreCSpec where

import Test.Hspec

import Swipl

spec :: Spec
spec = parallel . describe "Language.SecreC" $ do
    compilesSuccessfully "examples/ppdatalog/fib.pl" 2
    compilesSuccessfully "examples/ppdatalog/market.pl" 4
    compilesSuccessfully "examples/ppdatalog/auction.pl" 4
    compilesSuccessfully "examples/ppdatalog/employee.pl" 4
    compilesSuccessfully "examples/ppdatalog/relatives.pl" 3
    compilesSuccessfully "examples/ppdatalog/fib_unfolded_3_fulltyped.pl" 2
    compilesSuccessfully "examples/ppdatalog/aggregation.pl" 4
    compilesSuccessfully "examples/ppdatalog/market_unfolded_fulltyped.pl" 4
    compilesSuccessfully "examples/ppdatalog/employee_unfolded_fulltyped.pl" 4
    compilesSuccessfully "examples/ppdatalog/relatives_unfolded_3_fulltyped.pl" 3
    compilesSuccessfully "examples/ppdatalog/ship.pl" 4
    compilesSuccessfully "examples/ppdatalog/ship_short.pl" 4
    compilesSuccessfully "examples/ppdatalog/precendence.pl" 4
    compilesSuccessfully "examples/ppdatalog/disjunction.pl" 4
