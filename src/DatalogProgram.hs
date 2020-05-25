{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DatalogProgram
  ( DatalogProgram
  , Goal
  , IsGoal, toGoal
  , DBClause
  , LogicProgram
  , makeGoal
  , rules, goal
  , mapRules, setRules
  , inputs, outputs, formula
  , toDatalogSource
  , fromRulesAndGoal
  , dbClause
  , ruleNames
  , rulesByName
  , ruleLens
  , ppDatalogProgram
  ) where

import qualified Data.Map as M

import           Data.List
import           Data.Maybe

import           Control.Monad (guard)

import           Control.Lens

import           Rule
import           Expr
import           DBClause
import           Data.Text.Prettyprint.Doc

class LogicProgram a where
  rules       :: a -> [Rule]
  goal        :: a -> Maybe Goal
  ruleNames   :: a -> [String]
  rulesByName :: a -> String -> [Rule]
  mapRules    :: (Rule -> Rule) -> a -> a
  setRules    :: [Rule] -> a -> a

  ruleNames prog = nub $ name <$> rules prog
  rulesByName prog n =
    do
      f <- rules prog
      guard $ name f == n
      return f

class IsGoal a where
  toGoal :: a -> Goal

data Goal = Goal
  { _gInputs  :: [Expr DBVar]
  , _gOutputs :: [Expr DBVar]
  , _gFormula :: Expr DBVar
  }
  deriving (Show)

instance Pretty Goal where
  pretty g = 
       "goal(["
    <> (hsep . punctuate "," $ pretty <$> _gInputs g)
    <> "],["
    <> (hsep . punctuate "," $ pretty <$> _gOutputs g)
    <> "]) :-\n"
    <> indent 2 (pretty $ _gFormula g)


instance IsGoal Goal where
  toGoal = id

data DatalogProgram = DatalogProgram
  { _dpRules :: M.Map String [Rule]
  , _dpGoal  :: Maybe Goal
  , _ppDBClauses :: [DBClause]
  }
  deriving (Show)
makeLenses ''DatalogProgram

instance Pretty DatalogProgram where
  pretty p =
    (hcat $ (<>";\n\n") . pretty <$> rules p) <>
    (fromMaybe emptyDoc (pretty <$> goal p))

genRuleMap :: [Rule] -> M.Map String [Rule]
genRuleMap rs = M.unionsWith (<>) $ f <$> rs
  where
    f x = M.singleton (name x) [x]

instance LogicProgram DatalogProgram where
  rules = concat . M.elems . _dpRules
  goal  = _dpGoal
  mapRules f = dpRules %~ ((f <$>) <$>)
  setRules r = dpRules .~ genRuleMap r

makeGoal ::
     [Expr DBVar]
  -> [Expr DBVar]
  -> Expr DBVar
  -> Goal
makeGoal = Goal

inputs :: Goal -> [Expr DBVar]
inputs = _gInputs

outputs :: Goal -> [Expr DBVar]
outputs = _gOutputs

formula :: Goal -> Expr DBVar
formula = _gFormula

toDatalogSource :: DatalogProgram -> String
toDatalogSource  = undefined

fromRulesAndGoal :: [Rule] -> Maybe Goal -> DatalogProgram
fromRulesAndGoal rs g = DatalogProgram (genRuleMap rs) g []

ruleLens :: Traversal' DatalogProgram [Rule]
ruleLens = dpRules . traversed

ppDatalogProgram :: [Rule] -> Maybe Goal -> [DBClause] -> DatalogProgram
ppDatalogProgram r = DatalogProgram (genRuleMap r)

