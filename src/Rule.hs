{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rule
  ( Rule
  , rule, fact
  , args
  , ruleHead, ruleTail
  ) where

---------------------------------------------------------
---- Data structures for LP facts and rules
---------------------------------------------------------

import           Data.Text.Prettyprint.Doc

import           Control.Lens

import           Expr
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
    ":-" <+>  
    hardline <+>
    (indent 2 $ pretty $ _ruleTail r)

fact :: String -> [Expr D.DBVar] -> Rule
fact n args = Rule (Pred n args) (ConstBool True)

rule :: String -> [Expr D.DBVar] -> Expr D.DBVar -> Rule
rule n args p = Rule (Pred n args) p

args :: Rule -> [Expr D.DBVar]
args = view $ ruleHead . _Pred . _2

