{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DatalogProgram
  ( DatalogProgram
  , Directive
  , DBClause
  , fromRulesAndGoal
  , ruleLens
  --, dbClauseLens
  , ppDatalogProgram
  , programGoalExpr
  , dpRules
  , dpDBClauses
  , dpGoal
  , directive
  , prolog
  , goal, inputs, outputs
  , intentionalFacts
  , extensionalFacts
  , facts
  ) where

import           Control.Lens hiding (List)

import           Rule
import           Expr
import           Data.Text.Prettyprint.Doc
import           DBClause

class PrologSource a where
  -- | Pretty-print data as valid prolog source code
  prolog :: a -> Doc ann

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
  , _dpGoal       :: Expr
  , _dpDirectives :: [Directive]
  }
  deriving (Show)

makeLenses ''DatalogProgram

instance Pretty DatalogProgram where
  pretty p = vsep
    [ hcat $ (<>".\n\n") . pretty <$> _dpDirectives p
    , hcat $ (<>".\n\n") . pretty <$> _dpRules p
    , (pretty $ _dpGoal p) <> "?"
    ]

instance PrologSource DatalogProgram where
  prolog dp = vsep
    [ vsep $ prolog <$> dp ^. dpRules
    , prologGoal (_dpGoal dp) (inputs dp) (outputs dp)
    ]

instance PrologSource Rule where
  prolog x = pretty x <> "."

instance PrologSource Expr where
  prolog = pretty

instance PrologSource Directive where
  prolog _ = emptyDoc

fromRulesAndGoal :: [Rule] -> Expr -> DatalogProgram
fromRulesAndGoal rs g = DatalogProgram rs g []

ruleLens :: Traversal' DatalogProgram [Rule]
ruleLens = dpRules

ppDatalogProgram :: [Rule] -> Expr -> [Directive] -> DatalogProgram
ppDatalogProgram r = DatalogProgram r

programGoalExpr :: DatalogProgram -> Expr
programGoalExpr = view dpGoal

directive :: String -> [Expr] -> Directive
directive = Directive

dpDBClauses :: Fold DatalogProgram DBClause
dpDBClauses = dpDirectives . folded . to(dirToDBC) . _Just

-- | Attempts to convert a directive to DBClause
dirToDBC :: Directive -> Maybe DBClause
dirToDBC (Directive "type" [(ConstStr _ n), (List _ as)]) = Just $ dbClause n as
dirToDBC _ = Nothing

goal :: DatalogProgram -> Expr
goal = _dpGoal

xputs :: String -> DatalogProgram -> [Expr]
xputs predn dp = dp ^.. dpDirectives . folded . to(f) . folded
  where
    f :: Directive -> [Expr]
    f (Directive _predn [List _ as]) 
      | predn == _predn = as
      | otherwise       = []
    f _ = []

inputs :: DatalogProgram -> [Expr]
inputs = xputs "inputs"

outputs :: DatalogProgram -> [Expr]
outputs = xputs "outputs"

prologGoal :: Expr -> [Expr] -> [Expr] -> Doc ann
prologGoal formula ins outs = hcat
  [ "goal(["
  , cat . punctuate ", " $ prolog <$> ins
  , "],["
  , cat . punctuate ", " $ prolog <$> outs
  , "]) :- "
  , prolog formula
  , "."
  ]

intentionalFacts :: DatalogProgram -> [Rule]
intentionalFacts dp = dp ^.. dpRules . folded . filtered(fil)
  where fil = view $ ruleTail . to(==constTrue)

extensionalFacts :: DatalogProgram -> [Rule]
extensionalFacts dp = dp ^.. dpDirectives 
                           . folded 
                           . to dirToDBC 
                           . _Just 
                           . to dbClauseToRule

facts :: DatalogProgram -> [Rule]
facts dp = intentionalFacts dp <> extensionalFacts dp

