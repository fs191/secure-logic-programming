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
  , findRules
  , findDBFact
  ) where

import           Control.Lens hiding (List)

import           Data.Maybe

import           Rule
import           Expr
import           Data.Text.Prettyprint.Doc
import           DBClause
import           Language.Prolog.PrologSource

data Directive
  = InputDirective ![Expr]
  | OutputDirective ![Expr]
  | DBDirective String ![Expr]
  deriving (Show, Eq)

instance Pretty Directive where
  pretty (InputDirective as) = ":-inputs([" <> _ps <> "])"
    where _ps = cat . punctuate ", " $ pretty <$> as
  pretty (OutputDirective as) = ":-outputs([" <> _ps <> "])"
    where _ps = cat . punctuate ", " $ pretty <$> as
  pretty (DBDirective n as) = ":-type(" <> pretty n <> ",[" <> _ps <> "])"
    where _ps = cat . punctuate ", " $ pretty <$> as

data DatalogProgram = DatalogProgram
  { _dpRules      :: ![Rule]
  , _dpGoal       :: !Expr
  , _dpDirectives :: ![Directive]
  }
  deriving (Show, Eq)

makeLenses ''DatalogProgram
makePrisms ''Directive

instance Pretty DatalogProgram where
  pretty p = vsep
    [ hcat $ (<>".\n\n") . pretty <$> _dpDirectives p
    , hcat $ (<>".\n\n") . pretty <$> _dpRules p
    , pretty (_dpGoal p) <> "?"
    ]

instance PrologSource DatalogProgram where
  prolog dp = vsep
    [ vsep $ prolog <$> dp ^. dpRules
    , prologGoal (_dpGoal dp) (dp ^.. inputs) $ dp ^.. outputs
    ]


instance PrologSource Directive where
  prolog _ = emptyDoc

fromRulesAndGoal :: [Rule] -> Expr -> DatalogProgram
fromRulesAndGoal rs g = DatalogProgram rs g []

ruleLens :: Traversal' DatalogProgram [Rule]
ruleLens = dpRules

ppDatalogProgram :: [Rule] -> Expr -> [Directive] -> DatalogProgram
ppDatalogProgram = DatalogProgram

programGoalExpr :: DatalogProgram -> Expr
programGoalExpr = view dpGoal

dpDBClauses :: Fold DatalogProgram DBClause
dpDBClauses = dpDirectives . folded . to dirToDBC . _Just

-- | Attempts to convert a directive to DBClause
dirToDBC :: Directive -> Maybe DBClause
dirToDBC (DBDirective n as) = Just $ dbClause n as
dirToDBC _ = Nothing

goal :: DatalogProgram -> Expr
goal = _dpGoal

inputs :: Traversal' DatalogProgram Expr
inputs = dpDirectives . traversed . _InputDirective . traversed . attrToVar

outputs :: Traversal' DatalogProgram Expr
outputs = dpDirectives . traversed . _OutputDirective . traversed

attrToVar :: Iso' Expr Expr
attrToVar = iso (\(Attribute e x) -> Var e x) (\(Var e x) -> Attribute e x)

prologGoal :: Expr -> [Expr] -> [Expr] -> Doc ann
prologGoal formula ins outs = hcat
  [ "goal(["
  , cat . punctuate ", " $ prolog . variablize <$> ins
  , "],["
  , cat . punctuate ", " $ prolog <$> outs
  , "]) :- "
  , prolog . variablizeInputs (catMaybes $ identifier <$> ins) $ formula
  , "."
  ]

intentionalFacts :: DatalogProgram -> [Rule]
intentionalFacts dp = dp ^.. dpRules . folded . filtered fil
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

findRules :: DatalogProgram -> String -> [Rule]
findRules dp n = dp ^.. dpRules . folded . filtered f
  where f x = fromMaybe False $ x ^? ruleHead . _Pred . _2 . to (==n)

variablize :: Expr -> Expr
variablize (Var e n) = Var e $ "IN" <> n
variablize x = error $ "Attempting to variablize " ++ show x

variablizeInputs :: [String] -> Expr -> Expr
variablizeInputs ins (Pred e n xs) = Pred e n $ f <$> xs
  where 
    f v@(Attribute ann n') 
      | elem n' ins = Attribute ann $ "IN" <> n'
      | otherwise  = v
    f x = x
variablizeInputs _ x = error $ "Attempting to variablize " ++ show x

findDBFact :: DatalogProgram -> String -> Rule
findDBFact dp n = fromMaybe err $ extensionalFacts dp 
                    ^? folded 
                     . filtered ((==n) . ruleName)
  where
    err = error $ "DB fact not found: " ++ show n

