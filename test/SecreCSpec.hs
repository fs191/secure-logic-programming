module SecreCSpec where

import Relude

import Test.Hspec

import Swipl

spec :: Spec
spec = parallel . describe "Language.SecreC" $ do
    compilesSuccessfully "examples/ppdatalog/fib.pl" 4
    compilesSuccessfully "examples/ppdatalog/market.pl" 4
    compilesSuccessfully "examples/ppdatalog/auction.pl" 4
    compilesSuccessfully "examples/ppdatalog/employee.pl" 4
    compilesSuccessfully "examples/ppdatalog/relatives.pl" 3
    compilesSuccessfully "examples/ppdatalog/fib_unfolded_3_fulltyped.pl" 2
    compilesSuccessfully "examples/ppdatalog/aggregation.pl" 7
    compilesSuccessfully "examples/ppdatalog/market_unfolded_fulltyped.pl" 4
    compilesSuccessfully "examples/ppdatalog/employee_unfolded_fulltyped.pl" 4
    compilesSuccessfully "examples/ppdatalog/relatives_unfolded_3_fulltyped.pl" 3
    compilesSuccessfully "examples/ppdatalog/ship.pl" 15
    compilesSuccessfully "examples/ppdatalog/ship_short.pl" 5
    compilesSuccessfully "examples/ppdatalog/precendence.pl" 5
    compilesSuccessfully "examples/ppdatalog/disjunction.pl" 5
    compilesSuccessfully "examples/ppdatalog/nondet.pl" 5
    compilesSuccessfully "examples/ppdatalog/sqrt.pl" 5
    compilesSuccessfully "examples/ppdatalog/privacy_labels.pl" 5
    compilesSuccessfully "examples/ppdatalog/ship_mintime.pl" 5
    emulatorGivesCorrectAnswer "examples/ppdatalog/fib.pl" "true\n[2]\n" 4
    emulatorGivesCorrectAnswer "examples/ppdatalog/market.pl" "djslgkjg" 4
    emulatorGivesCorrectAnswer "examples/ppdatalog/auction.pl" "true\n[dave]\n[eve]\n" 4
    emulatorGivesCorrectAnswer "examples/ppdatalog/employee.pl" "true\n[bob,0]\n[henry,1200]\n[victor,1900]\n[alice,2000]\n[peggy,2100]\n" 4
    emulatorGivesCorrectAnswer "examples/ppdatalog/relatives.pl" "true\n[dave]\n[chris]\n" 3
    emulatorGivesCorrectAnswer "examples/ppdatalog/fib_unfolded_3_fulltyped.pl" "kljds" 2
    emulatorGivesCorrectAnswer "examples/ppdatalog/aggregation.pl" "true\n[0]\n" 7
    emulatorGivesCorrectAnswer "examples/ppdatalog/market_unfolded_fulltyped.pl" "lsdjg" 4
    emulatorGivesCorrectAnswer "examples/ppdatalog/employee_unfolded_fulltyped.pl" "true\n[bob,0]\n[henry,1200]\n[victor,1900]\n[alice,2000]\n[peggy,2100]\n" 4
    emulatorGivesCorrectAnswer "examples/ppdatalog/relatives_unfolded_3_fulltyped.pl" "fjdslkj" 3
    emulatorGivesCorrectAnswer "examples/ppdatalog/ship.pl" "JFLj" 15
    emulatorGivesCorrectAnswer "examples/ppdatalog/ship_short.pl" "jlsdfj" 5
    emulatorGivesCorrectAnswer "examples/ppdatalog/precendence.pl" "true\n[0.0]\n" 5
    emulatorGivesCorrectAnswer "examples/ppdatalog/disjunction.pl" "jsdlj" 5
    emulatorGivesCorrectAnswer "examples/ppdatalog/nondet.pl" "sjflj" 5
    emulatorGivesCorrectAnswer "examples/ppdatalog/sqrt.pl" "true\n[0.0]\n" 5
    emulatorGivesCorrectAnswer "examples/ppdatalog/privacy_labels.pl" "jlkdsjg" 5
    emulatorGivesCorrectAnswer "examples/ppdatalog/ship_mintime.pl" "jdslkjg" 5

