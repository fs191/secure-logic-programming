module SecreCSpec where

import Test.Hspec

import Swipl

spec :: Spec
spec = parallel . describe "Language.SecreC" $ do
    compilesSuccessfully "examples/ppdatalog/fib.pl"
    compilesSuccessfully "examples/ppdatalog/market.pl"
    compilesSuccessfully "examples/ppdatalog/auction.pl"
    compilesSuccessfully "examples/ppdatalog/employee.pl"
    compilesSuccessfully "examples/ppdatalog/relatives.pl"
    compilesSuccessfully "examples/ppdatalog/fib_unfolded_3_fulltyped.pl"
    compilesSuccessfully "examples/ppdatalog/aggregation.pl"
    compilesSuccessfully "examples/ppdatalog/market_unfolded_fulltyped.pl"
    compilesSuccessfully "examples/ppdatalog/employee_unfolded_fulltyped.pl"
    compilesSuccessfully "examples/ppdatalog/relatives_unfolded_3_fulltyped.pl"
    compilesSuccessfully "examples/ppdatalog/ship.pl"
    compilesSuccessfully "examples/ppdatalog/ship_short.pl"
    compilesSuccessfully "examples/ppdatalog/precendence.pl"
    compilesSuccessfully "examples/ppdatalog/disjunction.pl"
