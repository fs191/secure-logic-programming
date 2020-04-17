{-# LANGUAGE TemplateHaskell #-}

module DatalogProgram
  ( DatalogProgram
  , Goal
  , makeProgram
  , makeGoal
  , facts, rules, goal
  , inputs, outputs, formulae
  ) where

import qualified Data.Map as M

import           Optics.Optic
import           Optics.TH

import           Rule

data Goal = Goal
  { _gInputs   :: [Term]
  , _gOutputs  :: [Term]
  , _gFormulae :: [Formula]
  }
  deriving (Show)

data DatalogProgram = DatalogProgram
  { _dpFacts :: M.Map PName PMap
  , _dpRules :: M.Map PName [Rule]
  , _dpGoal  :: Maybe Goal
  }
  deriving (Show)

makeProgram ::
     M.Map PName PMap
  -> M.Map PName [Rule]
  -> Maybe Goal
  -> DatalogProgram
makeProgram = DatalogProgram

makeGoal ::
     [Term]
  -> [Term]
  -> [Formula]
  -> Goal
makeGoal = Goal

facts :: DatalogProgram -> M.Map PName PMap
facts = _dpFacts

rules :: DatalogProgram -> M.Map PName [Rule]
rules = _dpRules

goal :: DatalogProgram -> Maybe Goal
goal = _dpGoal

inputs :: Goal -> [Term]
inputs = _gInputs

outputs :: Goal -> [Term]
outputs = _gOutputs

formulae :: Goal -> [Formula]
formulae = _gFormulae
