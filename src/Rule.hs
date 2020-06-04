{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rule
  ( Rule
  , rule, fact
  , args
  , isFact
  , ruleHead, ruleTail
  , refreshRule
  , applySubst
  ) where

---------------------------------------------------------
---- Data structures for LP facts and rules
---------------------------------------------------------

import           Data.Text.Prettyprint.Doc

import           Control.Lens

import           Expr
import           Substitution

-- a rule has a list of arguments and a formula that represents rule premise

data Rule = Rule
  { _ruleHead :: Expr
  , _ruleTail :: Expr
  }
  deriving (Eq)
makeLenses ''Rule

instance Pretty Rule where
  pretty (Rule h (ConstBool _ True)) =
    (pretty h) <> "."
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
refreshRule prefix r = applySubst s r
  where 
    s = mconcat $ refreshExpr prefix <$> [r ^. ruleHead, r ^. ruleTail]

applySubst :: Subst -> Rule -> Rule
applySubst s r = r & ruleHead %~ applyToExpr s
                   & ruleTail %~ applyToExpr s

