{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DatalogProgram
  ( DatalogProgram
  , Goal, Directive
  , IsGoal, toGoal
  , DBClause
  , LogicProgram
  , makeGoal
  , rules, goal
  , setRules
  , inputs, outputs, formula
  , toDatalogSource
  , fromRulesAndGoal
  , ruleLens
  --, dbClauseLens
  , ppDatalogProgram
  , programGoalExpr
  , dpRules
  , dpDBClauses
  , gFormula, gInputs, gOutputs
  , dpGoal
  , directive
  ) where

import           Data.Maybe

import           Control.Lens hiding (List)

import           Rule
import           Expr
import           Data.Text.Prettyprint.Doc
import           DBClause

class LogicProgram a where
  rules       :: a -> [Rule]
  goal        :: a -> Maybe Goal
  setRules    :: [Rule] -> a -> a

class IsGoal a where
  toGoal :: a -> Goal

data Goal = Goal
  { _gInputs  :: [Expr]
  , _gOutputs :: [Expr]
  , _gFormula :: Expr
  }
  deriving (Show)
makeLenses ''Goal

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

data Directive = Directive String [Expr]
  deriving (Show)

instance Pretty Directive where
  pretty (Directive n as) = hsep
    [ ":-"
    , pretty n
    , tupled $ pretty <$> as
    ]

data DatalogProgram = DatalogProgram
  { _dpRules      :: [Rule]
  , _dpGoal       :: Maybe Goal
  , _dpDirectives :: [Directive]
  }
  deriving (Show)

makeLenses ''DatalogProgram

instance Pretty DatalogProgram where
  pretty p =
    (hcat $ (<>".\n\n") . pretty <$> _dpDirectives p) <>
    (hcat $ (<>".\n\n") . pretty <$> rules p) <>
    (fromMaybe emptyDoc $ do
       g <- pretty <$> goal p
       return $ g <> ".")

instance LogicProgram DatalogProgram where
  rules = _dpRules
  goal  = _dpGoal
  setRules r = dpRules .~ r

makeGoal ::
     [Expr]
  -> [Expr]
  -> Expr
  -> Goal
makeGoal = Goal

inputs :: Goal -> [Expr]
inputs = _gInputs

outputs :: Goal -> [Expr]
outputs = _gOutputs

formula :: Goal -> Expr
formula = _gFormula

toDatalogSource :: DatalogProgram -> String
toDatalogSource  = undefined

fromRulesAndGoal :: [Rule] -> Maybe Goal -> DatalogProgram
fromRulesAndGoal rs g = DatalogProgram rs g []

ruleLens :: Traversal' DatalogProgram [Rule]
ruleLens = dpRules

ppDatalogProgram :: [Rule] -> Maybe Goal -> [Directive] -> DatalogProgram
ppDatalogProgram r = DatalogProgram r

programGoalExpr :: DatalogProgram -> Maybe Expr
programGoalExpr = (^? dpGoal . _Just . gFormula)

directive :: String -> [Expr] -> Directive
directive = Directive

dpDBClauses :: Fold DatalogProgram DBClause
dpDBClauses = dpDirectives . folded . to(dirToDBC) . _Just

-- | Attempts to convert a directive to DBClause
dirToDBC :: Directive -> Maybe DBClause
dirToDBC (Directive "type" [(ConstStr _ n), (List _ as)]) = Just $ dbClause n as
dirToDBC _ = Nothing

