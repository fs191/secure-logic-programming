module TransformSpec where

import Relude

import Test.Hspec

import Parser.DatalogParser

import Transform
import Swipl
import Expr
import DatalogProgram
import TestResults

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
    transPreserveSem "examples/prolog/relatives.pl" 1
    transPreserveSem "examples/ppdatalog/precendence.pl" 3
    transPreserveSem "examples/ppdatalog/aggregation.pl" 1
    transPreserveSemDB "examples/ppdatalog/market.pl" 3 marketDB
    transPreserveSemDB "examples/ppdatalog/auction.pl" 3 marketDB
    transPreserveSemDB "examples/ppdatalog/employee.pl" 3 employeeDB
    runsSuccessfully "examples/prolog/fib.pl" transform ["[2]"]
    runsSuccessfully "examples/prolog/market.pl" transform marketRes
    runsSuccessfully "examples/prolog/auction.pl" transform marketRes
    runsSuccessfully "examples/prolog/employee.pl" transform employeeRes
    runsSuccessfully "examples/ppdatalog/precendence.pl" transform ["[390625.0]"]
    runsSuccessfullyDB "examples/ppdatalog/market.pl" transform marketRes marketDB
    runsSuccessfullyDB "examples/ppdatalog/auction.pl" transform marketRes marketDB
    runsSuccessfullyDB "examples/ppdatalog/employee.pl" transform employeeRes employeeDB
    
transform :: DatalogProgram -> IO DatalogProgram
transform = deriveAllGroundRules 2

canDeriveOn :: Text -> Int -> Spec
canDeriveOn file n = it (show desc) $ action `shouldReturn` ()
  where 
    desc = "can derive ground rules on " <> file <> " with " <> show n <> " iterations"
    action = do
        let err = error "Cannot parse file"
        f <- fromRight err <$> parseDatalogFromFile file
        d <- deriveAllGroundRules n f
        return $ d `seq` ()
     
transPreserveSem :: Text -> Int -> Spec
transPreserveSem f n = 
  preservesSemantics (deriveAllGroundRules n) f

transPreserveSemDB :: Text -> Int -> [Expr] -> Spec
transPreserveSemDB f n = 
  preservesSemanticsDB (deriveAllGroundRules n) f

