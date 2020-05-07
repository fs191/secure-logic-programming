{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DatalogProgram
  ( DatalogProgram
  , PPDatalogProgram
  , Goal
  , IsGoal, toGoal
  , DBClause
  , LogicProgram
  , makeGoal
  , facts, rules, goal
  , mapRules, setRules
  , inputs, outputs, formula
  , toDatalogSource
  , fromRulesAndGoal
  , ppDatalogProgram
  , dbClause
  , ruleNames
  , rulesByName
  ) where

import qualified Data.Map as M

import           Data.List (nub)

import           Control.Monad (guard)

import           Optics.Operators
import           Optics.TH

import           Rule
import           Table
import           DBClause

class LogicProgram a where
  rules     :: a -> [Rule]
  facts     :: a -> [Fact]
  goal      :: a -> Maybe Goal
  ruleNames :: a -> [String]
  rulesByName :: a -> String -> [Rule]
  mapRules :: (Rule -> Rule) -> a -> a
  setRules :: [Rule] -> a -> a

  facts = rulesToFacts . rules
  ruleNames prog = nub $ functor <$> rules prog
  rulesByName prog n =
    do
      f <- rules prog
      guard $ functor f == n
      return f

class IsGoal a where
  toGoal :: a -> Goal

data Goal = Goal
  { _gInputs   :: [Term]
  , _gOutputs  :: [Term]
  , _gFormula :: Formula
  }

instance Show Goal where
  show g =
    "Goal:\n\tInputs: \t"     ++ show (_gInputs g) ++
    "\n\tOutputs:\t"  ++ show (_gOutputs g) ++
    "\n\tFormulae:\t" ++ show (_gFormula g)

instance IsGoal Goal where
  toGoal = id

data DatalogProgram = DatalogProgram
  { _dpRules :: M.Map String [Rule]
  , _dpGoal  :: Maybe Goal
  }
  deriving (Show)
makeLenses ''DatalogProgram

data PPDatalogProgram = PPDatalogProgram
  { _ppProgram   :: DatalogProgram
  , _ppDBClauses :: [DBClause]
  }
  deriving (Show)

makeLenses ''PPDatalogProgram

instance LogicProgram DatalogProgram where
  rules = concat . M.elems . _dpRules
  goal  = _dpGoal
  mapRules f = dpRules %~ ((f <$>) <$>)
  setRules r = dpRules .~ genRuleMap r

instance LogicProgram PPDatalogProgram where
  rules = rules . _ppProgram
  goal = goal . _ppProgram
  mapRules f = ppProgram %~ mapRules f
  setRules r = ppProgram %~ setRules r

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
fromRulesAndGoal rs = DatalogProgram $ genRuleMap rs

ppDatalogProgram :: DatalogProgram -> [DBClause] -> PPDatalogProgram
ppDatalogProgram = PPDatalogProgram

genRuleMap :: [Rule] -> M.Map String [Rule]
genRuleMap rs = M.unionsWith (<>) $ f <$> rs
  where
    f x = M.singleton (functor x) [x]

