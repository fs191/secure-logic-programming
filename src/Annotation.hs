{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Annotation 
  ( Ann(..)
  , Binding(..), BindingPattern(..)
  , empty
  , annType, domain, bindings
  ) where

import Control.Lens

import Data.Data

import Language.SecreC.Types

newtype BindingPattern = BindingPattern [Binding]
  deriving (Eq, Ord, Data, Typeable)

instance Show BindingPattern where
  show = bindingPatternSuffix

bindingPatternSuffix :: BindingPattern -> String
bindingPatternSuffix (BindingPattern l) = bindingChar <$> l

bindingChar :: Binding -> Char
bindingChar Free  = 'f'
bindingChar Bound = 'b'

data Binding
  = Free
  | Bound
  deriving (Enum, Eq, Ord, Data, Typeable)

instance Show Binding where
  show = show . bindingChar

data Ann = Ann
  { 
    -- Datatype of the term
    _annType  :: Maybe PPType
    -- Security domain of the term
  , _domain   :: Maybe PPDomain
  , _bindings :: Maybe BindingPattern
  }
  deriving (Ord, Show, Eq, Data, Typeable)

empty :: Ann
empty = Ann Nothing Nothing Nothing

makeLenses ''Ann

