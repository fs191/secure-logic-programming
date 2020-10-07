{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DatalogProgram
  ( DatalogProgram
  , Aggregation
  , Directive
  , DBClause
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

import           Control.Lens hiding (List)

import           Data.Maybe
import           Data.Data

import           Rule
import           Expr
import           Data.Text.Prettyprint.Doc
import           DBClause
import           Language.Prolog.PrologSource

-- | Directives are the top-level function calls in Datalog.
data Directive
  = InputDirective ![Expr]
  | OutputDirective ![Expr]
  | DBDirective String ![Expr]
  deriving (Show, Eq, Data, Typeable)

instance Pretty Directive where
  pretty (InputDirective as) = ":-inputs([" <> _ps <> "])"
    where _ps = cat . punctuate ", " $ pretty <$> as
  pretty (OutputDirective as) = ":-outputs([" <> _ps <> "])"
    where _ps = cat . punctuate ", " $ pretty <$> as
  pretty (DBDirective n as) = ":-type(" <> pretty n <> ",[" <> _ps <> "])"
    where _ps = cat . punctuate ", " $ pretty <$> as

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
    , "?-" <> pretty (p ^. dpGoal) <> "."
    ]

instance PrologSource DatalogProgram where
  prolog dp = vsep
    [ vsep $ prolog <$> dp ^. dpRules
    , prologGoal (dp ^. dpGoal) (dp ^.. inputs) $ dp ^.. outputs
    ]

instance PrologSource Directive where
  prolog _ = emptyDoc

-- | Creates a new datalog program from rules and a goal
fromRulesAndGoal :: [Rule] -> Expr -> DatalogProgram
fromRulesAndGoal rs g = DatalogProgram rs g []

-- | Creates a new datalog program from rules, goal and directives
ppDatalogProgram :: [Rule] -> Expr -> [Directive] -> DatalogProgram
ppDatalogProgram = DatalogProgram

dpDBClauses :: Fold DatalogProgram DBClause
dpDBClauses = dpDirectives . folded . to dirToDBC . _Just

-- | Attempts to convert a directive to DBClause
dirToDBC :: Directive -> Maybe DBClause
dirToDBC (DBDirective n as) = Just $ dbClause n as
dirToDBC _ = Nothing

-- | Traverse all input directives for input variables
inputs :: Traversal' DatalogProgram Expr
inputs = dpDirectives . traversed . _InputDirective . traversed . attrToVar

-- | Traverse all output directives for output variables
outputs :: Traversal' DatalogProgram Expr
outputs = dpDirectives . traversed . _OutputDirective . traversed

-- | Isomorphism between variables and attributes
attrToVar :: Iso' Expr Expr
attrToVar = iso (\(Attribute e x) -> Var e x) (\(Var e x) -> Attribute e x)

-- | Prettyprints a goal so that it can be used for testing with swipl
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
                           . to dbClauseToRule

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
dbDirective :: String -> [Expr] -> Directive
dbDirective = DBDirective

-- | Finds all rules with name `n` from the program
findRules :: DatalogProgram -> String -> [Rule]
findRules dp n = dp ^.. dpRules . folded . filtered f
  where f x = fromMaybe False $ x ^? ruleHead . predName . to (==n)

-- | Changes the names of variables so that they are valid Prolog variable
-- names. Needed for running the program in swipl.
variablize :: Expr -> Expr
variablize (Var e n) = Var e $ "IN" <> n
variablize x = error $ "Attempting to variablize " ++ show x

-- | Changes the names of all attributes in the goal expression so that they
-- are valid Prolog variables.
variablizeInputs :: [String] -> Expr -> Expr
variablizeInputs ins (Pred e n xs) = Pred e n $ f <$> xs
  where 
    f v@(Attribute a n') 
      | elem n' ins = Attribute a $ "IN" <> n'
      | otherwise  = v
    f x = x
variablizeInputs _ x = error $ "Attempting to variablize " ++ show x

-- | Finds an extensional fact with the name `n` from the program
findDBFact :: DatalogProgram -> String -> Maybe Rule
findDBFact dp n = extensionalFacts dp 
                    ^? folded 
                     . filtered ((==n) . ruleName)
  where
    err = error $ "DB fact not found: " ++ show n ++ "\n" ++ show (pretty dp)

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
        <> (show $ pretty a)

isEDBFact :: DatalogProgram -> Expr -> Bool
isEDBFact dp (Pred{_predName=n}) = isJust $ findDBFact dp n
isEDBFact _ _ = False

isIDBFact :: DatalogProgram -> Expr -> Bool
isIDBFact dp (p@Pred{}) = not $ isEDBFact dp p
isIDBFact _ _ = False

