{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rule
  ( Rule
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
  ) where

---------------------------------------------------------
---- Data structures for LP facts and rules
---------------------------------------------------------

import           Data.Text.Prettyprint.Doc

import           Control.Lens hiding (transform)
import           Data.Generics.Uniplate.Data

import           Expr
import           Substitution
import           DBClause

-- a rule has a list of arguments and a formula that represents rule premise

data Rule = Rule
  { _ruleHead :: Expr
  , _ruleTail :: Expr
  }
  deriving (Eq, Ord)
makeLenses ''Rule

instance Pretty Rule where
  pretty (Rule h (ConstBool _ True)) = pretty h
  pretty r =
    (pretty $ r ^. ruleHead) <+>
    ":-" <+>  
    hardline <+>
    (indent 2 $ pretty $ _ruleTail r)

instance Show Rule where
  show = show . pretty

fact :: String -> [Expr] -> Rule
fact n as = Rule (predicate n as) (constTrue)

rule :: String -> [Expr] -> Expr -> Rule
rule n as p = Rule (predicate n as) p

args :: Rule -> [Expr]
args = view $ ruleHead . _Pred . _3

isFact :: Rule -> Bool
isFact r = _ruleTail r == constTrue

refreshRule :: String -> Rule -> Rule
refreshRule prefix r' = applySubst s r
  where 
    r = r' & ruleTail %~ safePrefix
           & ruleHead %~ safePrefix
    s = refreshExpr prefix . eAnd (r ^. ruleHead) $ r ^. ruleTail

-- | Prefixes variable names with gibberish
safePrefix :: Expr -> Expr
safePrefix e = transform f e
  where
    f (Var a n) = Var a $ "$!?" ++ n
    f x = x


applySubst :: Subst -> Rule -> Rule
applySubst s r = r & ruleHead %~ applyToExpr s
                   & ruleTail %~ applyToExpr s

dbClauseToRule :: DBClause -> Rule
dbClauseToRule dbc = Rule (predicate (name dbc) $ vars dbc) constTrue

ruleName :: Rule -> String
ruleName (Rule (Pred _ n _) _) = n

ruleAnn :: Rule -> Ann
ruleAnn (Rule (Pred ann _ _) _) = ann

ruleSchema :: Rule -> [Ann]
ruleSchema (Rule (Pred _ _ zs) _) = map getAnn zs

