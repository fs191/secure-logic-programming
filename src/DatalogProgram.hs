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
  , prolog
  , goal, inputs, outputs
  , intentionalFacts
  , extensionalFacts
  , facts
  , inputDirective
  , outputDirective
  , dbDirective
  ) where

import           Control.Lens hiding (List)

import           Rule
import           Expr
import           Data.Text.Prettyprint.Doc
import           DBClause
import           Language.Prolog.PrologSource

data Directive 
  = InputDirective [Expr]
  | OutputDirective [Expr]
  | DBDirective String [Expr]
  deriving (Show)

instance Pretty Directive where
  pretty (InputDirective as) = ":-inputs([" <> _ps <> "])."
    where _ps = cat . punctuate ", " $ pretty <$> as
  pretty (OutputDirective as) = ":-outputs([" <> _ps <> "])."
    where _ps = cat . punctuate ", " $ pretty <$> as
  pretty (DBDirective n as) = ":-type(" <> (pretty n) <> ",[" <> _ps <> "])."
    where _ps = cat . punctuate ", " $ pretty <$> as

data DatalogProgram = DatalogProgram
  { _dpRules      :: [Rule]
  , _dpGoal       :: Expr
  , _dpDirectives :: [Directive]
  }
  deriving (Show)

makeLenses ''DatalogProgram
makePrisms ''Directive

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

dpDBClauses :: Fold DatalogProgram DBClause
dpDBClauses = dpDirectives . folded . to(dirToDBC) . _Just

-- | Attempts to convert a directive to DBClause
dirToDBC :: Directive -> Maybe DBClause
dirToDBC (DBDirective n as) = Just $ dbClause n as
dirToDBC _ = Nothing

goal :: DatalogProgram -> Expr
goal = _dpGoal

inputs :: DatalogProgram -> [Expr]
inputs = toListOf $ dpDirectives . folded . _InputDirective . folded

outputs :: DatalogProgram -> [Expr]
outputs = toListOf $ dpDirectives . folded . _OutputDirective . folded

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

inputDirective :: [Expr] -> Directive
inputDirective = InputDirective

outputDirective :: [Expr] -> Directive
outputDirective = OutputDirective

dbDirective :: String -> [Expr] -> Directive
dbDirective = DBDirective

