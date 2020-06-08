{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SecreC.Types 
  ( PPType(..), PPDomain(..)
  , join
  ) where

import Data.Text.Prettyprint.Doc

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

instance Pretty PPType where
  pretty PPBool = "bool"
  pretty PPInt  = "int"
  pretty PPStr  = "string"
  pretty PPAuto = "auto"

instance Pretty PPDomain where
  pretty Public  = "public"
  pretty Private = "private"

join :: PPDomain -> PPDomain -> PPDomain
join Private _ = Private
join _ Private = Private
join _ _       = Public

