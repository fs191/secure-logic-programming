{-# LANGUAGE OverloadedStrings #-}

module TestResults where

import Data.Text

import Expr

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

relativesRes :: [Text]
relativesRes =
  [ "[chris]"
  , "[dave]"
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

relativesDB =
  [ predicate "par"
    [ constStr "chris"
    , constStr "alice"
    ]
  , predicate "par"
    [ constStr "chris"
    , constStr "bob"
    ]
  , predicate "par"
    [ constStr "dave"
    , constStr "eve"
    ]
  , predicate "par"
    [ constStr "dave"
    , constStr "bob"
    ]
  , predicate "par"
    [ constStr "bob"
    , constStr "peggy"
    ]
  , predicate "par"
    [ constStr "bob"
    , constStr "victor"
    ]
  ]
