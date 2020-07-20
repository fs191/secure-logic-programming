{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Annotation 
  ( Ann(..)
  , empty
  , annType, domain
  , annBound
  , unifyAnns
  ) where

import Control.Lens

import Data.Data
import Data.Foldable

import Language.SecreC.Types

data Ann = Ann
  { 
    -- Datatype of the term
    _annType  :: PPType
    -- Security domain of the term
  , _domain   :: PPDomain
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
            , if x ^. annBound then ", bound" else ""
            ]

empty :: Ann
empty = Ann PPAuto Unknown False

-- | Unify types and domains. Return Nothing if the types do not unify.
unifyAnns :: Ann -> Ann -> Ann
unifyAnns x y = x & domain  %~  unifyDomains (y ^. domain)
                  & annType %~ ty
  where
    ty a = unifyTypes (y ^. annType) a

