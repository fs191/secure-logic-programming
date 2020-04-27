{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DatalogProgram
  ( DatalogProgram
  , PPDatalogProgram
  , Goal
  , DBClause
  , LogicProgram
  , makeGoal
  , facts, goal
  , inputs, outputs, formulae
  , toDatalogSource
  , fromRulesAndGoal
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
  , _gFormulae :: [Formula]
  }

instance Show Goal where
  show g =
    "Goal:\n\tInputs: \t"     ++ (show $ _gInputs g) ++
    "\n\tOutputs:\t"  ++ (show $ _gOutputs g) ++
    "\n\tFormulae:\t" ++ (show $ _gFormulae g)

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

data DBClause = DBClause
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
  -> [Formula]
  -> Goal
makeGoal = Goal

inputs :: Goal -> [Term]
inputs = _gInputs

outputs :: Goal -> [Term]
outputs = _gOutputs

formulae :: Goal -> [Formula]
formulae = _gFormulae

toDatalogSource :: DatalogProgram -> String
toDatalogSource  = undefined

fromRulesAndGoal :: [Rule] -> Maybe Goal -> DatalogProgram
fromRulesAndGoal = undefined

ppDatalogProgram :: DatalogProgram -> [DBClause] -> PPDatalogProgram
ppDatalogProgram = undefined

