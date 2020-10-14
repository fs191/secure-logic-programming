{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rule
  ( Rule(..)
  , rule, fact
  , args
  , isFact
  , ruleHead, ruleTail
  , dbClauseToRule
  , refreshRule
  , applySubst
  , ruleAnn
  , ruleName
  , ruleSchema
  , boundVars
  , unboundVars
  , primaryKey
  , nonPKLens
  , pkIndex
  ) where

---------------------------------------------------------
---- Data structures for LP facts and rules
---------------------------------------------------------

import           Data.Data
import           Data.Text.Prettyprint.Doc

import           Control.Lens hiding (transform)

import           Language.Prolog.PrologSource

import           Expr
import           Substitution
import           DBClause
import           Annotation

-- | A rule has a list of arguments and a formula that represents rule premise
data Rule = Rule
  { _ruleHead :: !Expr
  , _ruleTail :: !Expr
  }
  deriving (Eq, Ord, Show, Typeable, Data)
makeLenses ''Rule

instance Pretty Rule where
  pretty (Rule h (ConstBool _ True)) = pretty h
  pretty r =
    pretty (r ^. ruleHead) <+>
    ":-" <+>
    hardline <+>
    indent 2 (pretty $ _ruleTail r)

instance PrologSource Rule where
  prolog (Rule h (ConstBool _ True)) = prolog h <> "."
  prolog r = 
    prolog (r ^. ruleHead) <+>
    ":-" <+>
    hardline <+>
    indent 2 (prolog $ _ruleTail r) <> "."

-- | Creates a new fact from name and arguments.
-- A fact is a rule with a body that always evaluates to true.
fact :: String -> [Expr] -> Rule
fact n as = Rule (predicate n as) constTrue

-- | Creates a new rule from a name, arguments and body.
rule :: String -> [Expr] -> Expr -> Rule
rule n as = Rule (predicate n as)

-- | Gets the arguments of the rule head
args :: Rule -> [Expr]
args = view $ ruleHead . _Pred . _3

-- | Returns `True` if the rule is a fact.
isFact :: Rule -> Bool
isFact r = _ruleTail r == constTrue

-- | Refresh all variable names in the rule
refreshRule :: String -> Rule -> Rule
refreshRule prefix r = applySubst s r
  where
    s = refreshExpr prefix . eAnd (r ^. ruleHead) $ r ^. ruleTail

-- | Apply a substitution to the head and body of a rule.
applySubst :: Subst -> Rule -> Rule
applySubst s r = r & ruleHead %~ applyToExpr s
                   & ruleTail %~ applyToExpr s

-- | Convert a database clause to a rule
dbClauseToRule :: DBClause -> Rule
dbClauseToRule dbc = Rule (predicate (name dbc) $ vars dbc) constTrue

-- | Get the name of the rule
ruleName :: Rule -> String
ruleName (Rule (Pred _ n _) _) = n
ruleName _ = error "Got a rule with a non-predicate head"

-- | Get the annotation of the rule head
ruleAnn :: Rule -> Ann
ruleAnn (Rule (Pred ann _ _) _) = ann
ruleAnn _ = error "Got a rule with a non-predicate head"

-- | Get the schema of a rule
ruleSchema :: Rule -> [Ann]
ruleSchema (Rule (Pred _ _ zs) _) = map (view annotation) zs
ruleSchema _ = error "Got a rule with a non-predicate head"

boundVars :: Rule -> [Expr]
boundVars = filter (view $ annotation . annBound) . args

unboundVars :: Rule -> [Expr]
unboundVars = filter (view $ annotation . annBound . to not) . args

-- Attempts to get the primary key of a DB rule if it exists
primaryKey' :: Getter Rule (Maybe (Expr, Int))
primaryKey' = to getter
  where
    getter r = 
      case pks r of
        []  -> Nothing
        [x] -> Just x
        _   -> error $ "Rule " <> show r <> " has multiple primary keys."
    pks :: Rule -> [(Expr, Int)]
    pks r = r ^..
      ruleHead 
      . predArgs 
      . to (`zip` [0..])
      . folded 
      . filtered (^. _1 . annotation . isPK)

primaryKey :: Getter Rule (Maybe Expr)
primaryKey = to . preview $ primaryKey' . _Just . _1

pkIndex :: Getter Rule (Maybe Int)
pkIndex = to . preview $ primaryKey' . _Just . _2

nonPKLens :: Traversal' Rule Expr
nonPKLens = undefined

