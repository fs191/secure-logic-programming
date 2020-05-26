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
import qualified DBClause as D

-- a rule has a list of arguments and a formula that represents rule premise

data Rule = Rule
  { _ruleHead :: Expr D.DBVar
  , _ruleTail :: Expr D.DBVar
  }
  deriving (Show)
makeLenses ''Rule

instance D.Named Rule where
  name       = view $ ruleHead . _Pred . _1
  rename n r = r & ruleHead . _Pred . _1 .~ n

instance Pretty Rule where
  pretty r =
    (pretty $ r ^. ruleHead . _Pred . _1) <+>
    tupled (pretty <$> r ^. ruleHead . _Pred . _2) <+>
    ":-" <+>  
    hardline <+>
    (indent 2 $ pretty $ _ruleTail r)

fact :: String -> [Expr D.DBVar] -> Rule
fact n as = Rule (Pred n as) (ConstBool True)

rule :: String -> [Expr D.DBVar] -> Expr D.DBVar -> Rule
rule n as p = Rule (Pred n as) p

args :: Rule -> [Expr D.DBVar]
args = view $ ruleHead . _Pred . _2

isFact :: Rule -> Bool
isFact r = _ruleTail r == ConstBool True

refreshRule :: String -> Rule -> Rule
refreshRule prefix r = applySubst s r
  where 
    s = mconcat $ refreshExpr prefix <$> [r ^. ruleHead, r ^. ruleTail]

applySubst :: Subst D.DBVar -> Rule -> Rule
applySubst s r = r & ruleHead %~ applyToExpr s
                   & ruleTail %~ applyToExpr s

