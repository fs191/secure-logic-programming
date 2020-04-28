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

import           Data.List (intercalate, nub)
import           Data.Maybe (fromMaybe, catMaybes)

import           Control.Monad (guard)

import           Optics.Operators
import           Optics.TH

import           Rule

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
makeLenses ''DatalogProgram

data PPDatalogProgram = PPDatalogProgram
  { _ppProgram   :: DatalogProgram
  , _ppDBClauses :: [DBClause]
  }
  deriving (Show)

data DBClause = DBClause String [DBVar]
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
fromRulesAndGoal rules = DatalogProgram $ genRuleMap rules

ppDatalogProgram :: DatalogProgram -> [DBClause] -> PPDatalogProgram
ppDatalogProgram = PPDatalogProgram

dbClause :: String -> [DBVar] -> DBClause
dbClause = DBClause

genRuleMap :: [Rule] -> M.Map String [Rule]
genRuleMap rules = (M.unionsWith (<>) $ f <$> rules)
  where
    f x = M.singleton (show $ functor x) [x]

