{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Datalog.Literal 
  ( Variable(..)
  , Term(..)
  , Literal(..)
  , _Lit, _Var, _ConstStr
  ) where

import Relude

import Control.Lens

import Data.Data
import Data.Text.Prettyprint.Doc

data Variable = Variable 
  { _vName :: String
  }
  deriving (Ord,Show,Eq,Data,Typeable)

data Term
  = Lit Literal
  | Var Variable
  | ConstInt Int
  | ConstBool Bool
  deriving (Ord,Show,Eq,Data,Typeable)

data Literal = Literal
  { _lName :: String
  , _lArgs :: [Term]
  }
  deriving (Ord,Show,Eq,Data,Typeable)

instance Pretty Term where
  pretty (Lit x)       = pretty x
  pretty (Var x)       = pretty x
  pretty (ConstInt x)  = pretty x
  pretty (ConstBool x) = pretty x

instance Pretty Literal where
  pretty (Literal n []) = pretty n
  pretty (Literal n vs) = pretty n <> (tupled $ pretty <$> vs)

instance Pretty Variable where
  pretty = pretty . _vName

makeLenses ''Term
makePrisms ''Term

_ConstStr :: Prism' Term String
_ConstStr = prism (Lit . flip Literal []) f
  where
    f (Lit (Literal n [])) = Right n
    f x                    = Left x

