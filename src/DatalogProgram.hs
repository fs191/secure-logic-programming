{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DatalogProgram
  ( DatalogProgram
  , Goal, Directive
  , DBClause
  , makeGoal
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

class DatalogSource a where
  datalog :: a -> Doc ann

data Goal = Goal
  { _gInputs  :: [Expr]
  , _gOutputs :: [Expr]
  , _gFormula :: Expr
  }
  deriving (Show)
makeLenses ''Goal

instance Pretty Goal where
  pretty g = vsep
    [ "inputs(" <> list (pretty <$> _gInputs g) <> ")."
    , "outputs(" <> list (pretty <$> _gOutputs g) <> ")."
    , pretty (_gFormula g) <> "?"
    ]

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
    (hcat $ (<>".\n\n") . pretty <$> _dpRules p) <>
    (fromMaybe emptyDoc $ do
       g <- pretty <$> _dpGoal p
       return $ g <> ".")

instance DatalogSource DatalogProgram where
  datalog = undefined

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

