{-# LANGUAGE DeriveDataTypeable #-}

module Language.SecreC.Types 
  ( PPType(..), PPDomain(..)
  , join
  ) where

import Data.Data

data PPType
  = PPBool
  | PPInt
  | PPStr
  | PPAuto
  deriving (Ord, Show, Eq, Data, Typeable)

data PPDomain
  = Public
  | Private
  deriving (Ord, Show, Eq, Data, Typeable)

join :: PPDomain -> PPDomain -> PPDomain
join Private _ = Private
join _ Private = Private
join _ _       = Public

