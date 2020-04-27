{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DatalogProgram
  ( DatalogProgram
  , PPDatalogProgram
  , Goal
  , DBClause
  , LogicProgram
  , makeGoal
  , facts, goal
  , inputs, outputs, formula
  , toDatalogSource
  , fromRulesAndGoal
  , ppDatalogProgram
  , dbClause
  ) where

import qualified Data.Map as M

import           Data.List (intercalate)
import           Data.Maybe (fromMaybe, catMaybes)

import           Optics.Optic
import           Optics.TH

import           Rule

class LogicProgram a where
  rules :: a -> [Rule]
  facts :: a -> [Fact]
  goal  :: a -> Maybe Goal

  facts = catMaybes . map toFact . rules

data Goal = Goal
  { _gInputs   :: [Term]
  , _gOutputs  :: [Term]
  , _gFormula :: Formula
  }

instance Show Goal where
  show g =
    "Goal:\n\tInputs: \t"     ++ (show $ _gInputs g) ++
    "\n\tOutputs:\t"  ++ (show $ _gOutputs g) ++
    "\n\tFormulae:\t" ++ (show $ _gFormula g)

data DatalogProgram = DatalogProgram
  { _dpRules :: M.Map PName [Rule]
  , _dpGoal  :: Maybe Goal
  }
  deriving (Show)

data PPDatalogProgram = PPDatalogProgram
  { _ppProgram   :: DatalogProgram
  , _ppDBClauses :: [DBClause]
  }
  deriving (Show)

data DBClause = DBClause String [DBVar]
  deriving (Show)

instance LogicProgram DatalogProgram where
  rules = concat . M.elems . _dpRules
  goal  = _dpGoal

instance LogicProgram PPDatalogProgram where
  rules = rules . _ppProgram
  goal = goal . _ppProgram

makeGoal ::
     [Term]
  -> [Term]
  -> Formula
  -> Goal
makeGoal = Goal

inputs :: Goal -> [Term]
inputs = _gInputs

outputs :: Goal -> [Term]
outputs = _gOutputs

formula :: Goal -> Formula
formula = _gFormula

toDatalogSource :: DatalogProgram -> String
toDatalogSource  = undefined

fromRulesAndGoal :: [Rule] -> Maybe Goal -> DatalogProgram
fromRulesAndGoal rules = DatalogProgram (M.unionsWith (<>) $ f <$> rules)
  where
    f x = M.singleton (show $ functor x) [x]

ppDatalogProgram :: DatalogProgram -> [DBClause] -> PPDatalogProgram
ppDatalogProgram = PPDatalogProgram

dbClause :: String -> [DBVar] -> DBClause
dbClause = DBClause

