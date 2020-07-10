{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Annotation 
  ( Ann(..)
  , Binding(..), BindingPattern(..)
  , empty
  , annType, domain, bindings
  , annBound
  ) where

import Control.Lens

import Data.Data
import Data.Foldable

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
    _annType  :: PPType
    -- Security domain of the term
  , _domain   :: PPDomain
  , _bindings :: Maybe BindingPattern
  , _annBound :: Bool
  }
  deriving (Ord, Eq, Data, Typeable)

makeLenses ''Ann

instance Show Ann where
  show x = "{" <> s <> "}"
    where s = asum 
            [ x ^. annType . to show
            , ", "
            , x ^. domain . to show
            , maybe "" (\a -> (", " <>) $ show a) $ x ^. bindings
            , if x ^. annBound then ", bound" else ""
            ]

empty :: Ann
empty = Ann PPAuto Unknown Nothing False

