module SecreCSpec where

import Relude

import Test.Hspec

import Swipl
import TestResults

spec :: Spec
spec = describe "Language.SecreC" . parallel $ do
    compilesSuccessfully "examples/ppdatalog/fib.pl" 4
    compilesSuccessfully "examples/ppdatalog/market.pl" 4
    compilesSuccessfully "examples/ppdatalog/auction.pl" 4
    compilesSuccessfully "examples/ppdatalog/employee.pl" 4
    compilesSuccessfully "examples/ppdatalog/relatives.pl" 3
    compilesSuccessfully "examples/ppdatalog/fib_unfolded_3_fulltyped.pl" 0
    compilesSuccessfully "examples/ppdatalog/aggregation.pl" 7
    compilesSuccessfully "examples/ppdatalog/market_unfolded_fulltyped.pl" 0
    compilesSuccessfully "examples/ppdatalog/employee_unfolded_fulltyped.pl" 0
    compilesSuccessfully "examples/ppdatalog/relatives_unfolded_3_fulltyped.pl" 0
    compilesSuccessfully "examples/ppdatalog/ship.pl" 15
    compilesSuccessfully "examples/ppdatalog/ship_short.pl" 5
    compilesSuccessfully "examples/ppdatalog/precendence.pl" 5
    compilesSuccessfully "examples/ppdatalog/disjunction.pl" 5
    compilesSuccessfully "examples/ppdatalog/nondet.pl" 5
    compilesSuccessfully "examples/ppdatalog/sqrt.pl" 5
    compilesSuccessfully "examples/ppdatalog/privacy_labels.pl" 5
    compilesSuccessfully "examples/ppdatalog/ship_mintime.pl" 5
    compilesSuccessfully "examples/ppdatalog/xor_cast.pl" 5
    emulatorGivesCorrectAnswer 
      "examples/ppdatalog/fib.pl"
      ["[2]"]
      4
      []
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/market.pl"
      marketRes
      4
      [ "x1=alice:public:string"
      ]
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/auction.pl"
      marketRes
      4
      []
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/employee.pl"
      employeeRes
      4
      []
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/relatives.pl"
      relativesRes
      3
      []
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/fib_unfolded_3_fulltyped.pl"
      ["[2]"]
      0
      [ "x=2:private:int32"
      ]
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/aggregation.pl"
      ["[15]"]
      7
      []
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/market_unfolded_fulltyped.pl"
      marketRes
      0
      [ "x=alice:public:string"
      ]
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/employee_unfolded_fulltyped.pl"
      employeeRes
      0
      []
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/relatives_unfolded_3_fulltyped.pl"
      relativesRes
      0
      [ "x1=dave:public:string"
      ]
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/ship.pl"
      ["[0.0]"]
      15
      [ "portname_in=alma:private:string"
      , "cargotype_in=garlic:private:string"
      ]
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/ship_short.pl"
      ["[0.0]"]
      5
      [ "portname_in=alma:private:string"
      , "cargotype_in=garlic:private:string"
      ]
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/precendence.pl"
      ["[0.0]"]
      5
      []
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/disjunction.pl"
      ["[frank]","[maria]","[tika]"]
      5
      []
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/nondet.pl"
      ["[maria,0]","[maria,1]","[maria,2]"]
      5
      []
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/sqrt.pl"
      ["[0.0]"]
      5
      [
      ]
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/privacy_labels.pl"
      [ "[dave,garlic]"
      , "[eve,garlic]"
      ]
      5
      [ "x1=alice:public:string"
      ]
    emulatorGivesCorrectAnswer
      "examples/ppdatalog/ship_mintime.pl"
      ["[0.0]"]
      5
      [ "queryportname=alma:private:string"
      , "querycargotype=garlic:private:string"
      ]

