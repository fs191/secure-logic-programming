{-# LANGUAGE OverloadedStrings #-}

module TransformSpec where

import Test.Hspec

import Data.Text

import Parser.DatalogParser

import Transform
import Swipl
import Expr

spec :: Spec
spec =
  parallel . describe "Transform.deriveAllGroundRules" $ do
    transPreserveSem "examples/prolog/fib.pl" 1
    transPreserveSem "examples/prolog/market_xvars.pl" 1
    transPreserveSem "examples/prolog/market.pl" 1
    transPreserveSem "examples/prolog/market.pl" 2
    transPreserveSem "examples/prolog/auction.pl" 1
    transPreserveSem "examples/prolog/auction.pl" 2
    transPreserveSem "examples/prolog/employee.pl" 1
    transPreserveSem "examples/prolog/employee.pl" 2
    transPreserveSemDB "examples/ppdatalog/market.pl" 3 marketDB
    transPreserveSemDB "examples/ppdatalog/auction.pl" 3 marketDB
    transPreserveSemDB "examples/ppdatalog/employee.pl" 3 employeeDB
    transPreserveSem "examples/prolog/relatives.pl" 1
    runsSuccessfully "examples/prolog/fib.pl" ["[2]"]
    runsSuccessfully "examples/prolog/market.pl"  marketRes
    runsSuccessfully "examples/prolog/auction.pl" marketRes
    runsSuccessfully "examples/prolog/employee.pl"  employeeRes
    runsSuccessfullyDB "examples/ppdatalog/market.pl"  marketRes marketDB
    runsSuccessfullyDB "examples/ppdatalog/auction.pl" marketRes marketDB
    runsSuccessfullyDB "examples/ppdatalog/employee.pl"  employeeRes employeeDB
    

canDeriveOn :: String -> Int -> Spec
canDeriveOn file n = it desc $ action `shouldReturn` ()
  where 
    desc = "can derive ground rules on " ++ file ++ " with " ++ show n ++ " iterations"
    action = do
        f <- parseDatalogFromFile file
        let d = deriveAllGroundRules f n
        return $ d `seq` ()
     
transPreserveSem :: String -> Int -> Spec
transPreserveSem f n = 
  preservesSemantics (flip deriveAllGroundRules n) f

transPreserveSemDB :: String -> Int -> [Expr] -> Spec
transPreserveSemDB f n db = 
  preservesSemanticsDB (flip deriveAllGroundRules n) f db


--
-- Expected results
--

marketRes :: [Text]
marketRes = 
  [ "[eve]" 
  , "[dave]"
  ]

employeeRes :: [Text]
employeeRes = 
  [ "[bob,0]"
  , "[alice,2000]"
  , "[henry,1200]"
  , "[peggy,2100]"
  , "[victor,1900]"
  ]

--
-- PPDatalog databases
--

marketDB :: [Expr]
marketDB =
  [ predicate "buys" 
    [ constStr "alice"
    , constStr "garlic"
    , constInt 40
    ]
  , predicate "buys"
    [ constStr "bob"
    , constStr "carrots"
    , constInt 70
    ]
  , predicate "sells"
    [ constStr "eve"
    , constStr "garlic"
    , constInt 30
    ]
  , predicate "sells"
    [ constStr "bob"
    , constStr "onions"
    , constInt 40
    ]
  , predicate "sells"
    [ constStr "chris"
    , constStr "garlic"
    , constInt 70
    ]
  , predicate "sells"
    [ constStr "dave"
    , constStr "garlic"
    , constInt 40
    ]
  ]

employeeDB :: [Expr]
employeeDB = 
  [ predicate "eds"
    [ constStr "alice"
    , constStr "research"
    , constInt 2000
    ]
  , predicate "eds"
    [ constStr "bob"
    , constStr "research"
    , constInt 2500
    ]
  , predicate "eds"
    [ constStr "chris"
    , constStr "development"
    , constInt 1000
    ]
  , predicate "eds"
    [ constStr "charlie"
    , constStr "sales"
    , constInt 1100
    ]
  , predicate "eds"
    [ constStr "dave"
    , constStr "development"
    , constInt 1500
    ]
  , predicate "eds"
    [ constStr "eve"
    , constStr "development"
    , constInt 1200
    ]
  , predicate "eds"
    [ constStr "henry"
    , constStr "development"
    , constInt 800
    ]
  , predicate "eds"
    [ constStr "henry"
    , constStr "research"
    , constInt 1200
    ]
  , predicate "eds"
    [ constStr "peggy"
    , constStr "research"
    , constInt 2100
    ]
  , predicate "eds"
    [ constStr "victor"
    , constStr "research"
    , constInt 1900
    ]
  , predicate "dm"
    [ constStr "research"
    , constStr "manager1"
    ]
  , predicate "dm"
    [ constStr "development"
    , constStr "manager2"
    ]
  , predicate "dm"
    [ constStr "sales"
    , constStr "manager3"
    ]
  ]
