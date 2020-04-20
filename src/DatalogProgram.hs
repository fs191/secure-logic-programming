{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DatalogProgram
  ( DatalogProgram
  , Goal
  , makeProgram
  , makeGoal
  , facts, rules, goal
  , inputs, outputs, formulae
  ) where

import qualified Data.Map as M

import           Data.List (intercalate)
import           Data.Maybe (fromMaybe)

import           Optics.Optic
import           Optics.TH

import           Rule

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
  { _dpFacts :: M.Map PName PMap
  , _dpRules :: M.Map PName [Rule]
  , _dpGoal  :: Maybe Goal
  }

instance Show DatalogProgram where
  show dp = concat
    [ "Facts:\n"
    , showFacts $ _dpFacts dp
    , "Rules:\n"
    , showRules $ _dpRules dp
    , showGoal $ _dpGoal dp
    ]

showFacts :: M.Map PName PMap -> String
showFacts m = concat $ f <$> M.toList m
  where f (k, v) = "\t" ++ k ++ "\n" ++ (showPMap v)

showPMap :: M.Map [Term] Formula -> String
showPMap m = concat $ f <$> M.toList m
  where f (k, v) = "\t\t(" ++ (show k) ++ ") :- " ++ (show v) ++ "\n"

showRules :: M.Map PName [Rule] -> String
showRules m = concat $ f <$> M.toList m
  where f (k, v) = "\t" ++ k ++ ":\n\t" ++ (intercalate "\n\t" $ show <$> v)

showGoal :: Maybe Goal -> String
showGoal g = fromMaybe "" $ show <$> g

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
