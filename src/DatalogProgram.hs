{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DatalogProgram
  ( DatalogProgram
  , Aggregation
  , Directive
  , fromRulesAndGoal
  , ppDatalogProgram
  , dpRules
  , dpDBClauses
  , dpGoal, dpFullGoal
  , prolog
  , inputs, outputs
  , intentionalFacts
  , extensionalFacts
  , facts
  , inputDirective
  , outputDirective
  , dbDirective
  , findRules
  , findDBFact
  , isEDBFact
  , isIDBFact
  ) where

import           Relude

import           Control.Lens hiding (List)

import           Data.Data

import           Rule
import           Expr
import           ExprPretty
import           Data.Text.Prettyprint.Doc
import           Language.Prolog.PrologSource

-- | Directives are the top-level function calls in Datalog.
data Directive
  = InputDirective ![Expr]
  | OutputDirective ![Expr]
  | DBDirective !Expr
  deriving (Show, Eq, Data, Typeable)

instance Pretty Directive where
  pretty (InputDirective as) = ":-inputs([" <> _ps <> "])"
    where _ps = cat . punctuate ", " $ prettyFull <$> as
  pretty (OutputDirective as) = ":-outputs([" <> _ps <> "])"
    where _ps = cat . punctuate ", " $ prettyFull <$> as
  pretty (DBDirective p@Pred{}) = ":-type(" <> prettyMinimal p <>")"
  pretty (DBDirective _) = error "DB directive with a non-predicate argument"

-- | A datatype for representing privalog programs. Consists of rules, a goal
-- and directives.
data DatalogProgram = DatalogProgram
  { _dpRules      :: ![Rule]
  , _dpFullGoal   :: !Expr
  , _dpDirectives :: ![Directive]
  }
  deriving (Show, Eq, Data, Typeable)

makeLenses ''DatalogProgram
makePrisms ''Directive

instance Pretty DatalogProgram where
  pretty p = vsep
    [ hcat $ (<>".\n\n") . pretty <$> _dpDirectives p
    , hcat $ (<>".\n\n") . pretty <$> _dpRules p
    , "?-" <> prettyFull (p ^. dpGoal) <> "."
    ]

instance PrologSource DatalogProgram where
  prolog dp = vsep
    [ vsep $ prolog <$> dp ^. dpRules
    , prologGoal (dp ^. dpGoal) (dp ^.. inputs . traversed) $ dp ^.. outputs
    ]

instance PrologSource Directive where
  prolog _ = emptyDoc

-- | Creates a new datalog program from rules and a goal
fromRulesAndGoal :: [Rule] -> Expr -> DatalogProgram
fromRulesAndGoal rs g = DatalogProgram rs g []

-- | Creates a new datalog program from rules, goal and directives
ppDatalogProgram :: [Rule] -> Expr -> [Directive] -> DatalogProgram
ppDatalogProgram = DatalogProgram

dpDBClauses :: Fold DatalogProgram Expr
dpDBClauses = dpDirectives . folded . to dirToDBC . _Just

-- | Attempts to convert a directive to DBClause
dirToDBC :: Directive -> Maybe Expr
dirToDBC (DBDirective p@Pred{}) = Just p
dirToDBC _ = Nothing

-- | Traverse all input directives for input variables
inputs :: Traversal' DatalogProgram [Expr]
inputs = dpDirectives . traversed . _InputDirective

-- | Traverse all output directives for output variables
outputs :: Traversal' DatalogProgram Expr
outputs = dpDirectives . traversed . _OutputDirective . traversed

-- | Prettyprints a goal so that it can be used for testing with swipl
prologGoal :: Expr -> [Expr] -> [Expr] -> Doc ann
prologGoal formula ins outs = hcat
  [ "goal(["
  , cat . punctuate ", " $ prolog . variablize <$> ins
  , "],["
  , cat . punctuate ", " $ prolog <$> outs
  , "]) :- "
  , prolog formula
  , "."
  ]

-- | Returns all intentional database facts (rules that always evaluate to true)
intentionalFacts :: DatalogProgram -> [Rule]
intentionalFacts dp = dp ^.. dpRules . folded . filtered fil
  where fil = view $ ruleTail . to(==constTrue)

-- | Returns all extensional database facts (rules generated from type 
-- directives)
extensionalFacts :: DatalogProgram -> [Rule]
extensionalFacts dp = dp ^.. dpDirectives
                           . folded
                           . to dirToDBC
                           . _Just
                           . to (`Rule` constTrue)

-- | Returns a list of both intentional and extensional facts of the program
facts :: DatalogProgram -> [Rule]
facts dp = intentionalFacts dp <> extensionalFacts dp

-- | Creates a new input directive
inputDirective :: [Expr] -> Directive
inputDirective = InputDirective

-- | Creates a new output directive
outputDirective :: [Expr] -> Directive
outputDirective = OutputDirective

-- | Creates a new database directive
dbDirective :: Expr -> Directive
dbDirective p = DBDirective p

-- | Finds all rules with name `n` from the program
findRules :: DatalogProgram -> Text -> [Rule]
findRules dp n = dp ^.. dpRules . folded . filtered f
  where f x = fromMaybe False $ x ^? ruleHead . predName . to (==n)

-- | Changes the names of variables so that they are valid Prolog variable
-- names. Needed for running the program in swipl.
variablize :: Expr -> Expr
variablize (Var e n) = Var e $ "IN" <> n
variablize x = error $ "Attempting to variablize " <> show x

-- | Finds an extensional fact with the name `n` from the program
findDBFact :: DatalogProgram -> Text -> Maybe Rule
findDBFact dp n = extensionalFacts dp 
                    ^? folded 
                     . filtered ((==n) . ruleName)

dpGoal :: Lens' DatalogProgram Expr
dpGoal = lens getter setter
  where 
    getter dp = 
      case dp ^. dpFullGoal of
        p@Pred{}            -> p
        Aggr{_aggrPred = p} -> p
        x                   -> err x
    setter dp x =
      case dp ^. dpFullGoal of
        Pred{} -> dp & dpFullGoal .~ x
        Aggr{} -> dp & dpFullGoal . aggrPred .~ x
        a      -> err a
    err a =
      error $ "Invalid goal expression. Expected a predicate or an aggregation, got"
        <> (show $ prettyMinimal a)

isEDBFact :: DatalogProgram -> Expr -> Bool
isEDBFact dp (Pred{_predName=n}) = isJust $ findDBFact dp n
isEDBFact _ _ = False

isIDBFact :: DatalogProgram -> Expr -> Bool
isIDBFact dp (p@Pred{}) = not $ isEDBFact dp p
isIDBFact _ _ = False

