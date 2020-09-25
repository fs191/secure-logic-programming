{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Privalog.Types 
  ( PPType(..), PPDomain(..)
  , unifyTypes
  , unifyDomains
  ) where

import Data.Text.Prettyprint.Doc

import Data.Data

data PPType
  = PPBool
  | PPInt
  | PPStr
  | PPFloat
  | PPAuto
  deriving (Ord, Eq, Data, Typeable)

data PPDomain
  = Public
  | Private
  | Unknown
  deriving (Ord, Eq, Data, Typeable)

instance Show PPType where
  show PPBool  = "bool"
  show PPInt   = "int"
  show PPStr   = "string"
  show PPFloat = "float"
  show PPAuto  = "auto"

instance Pretty PPType where
  pretty = pretty . show

instance Show PPDomain where
  show Public  = "public"
  show Private = "private"
  show Unknown = "unknown"

instance Pretty PPDomain where
  pretty = pretty . show

unifyTypes :: PPType -> PPType -> Maybe PPType
unifyTypes x y | x == y  = Just x
unifyTypes PPAuto x      = Just x
unifyTypes x PPAuto      = Just x
unifyTypes PPFloat PPInt = Just PPFloat
unifyTypes PPInt PPFloat = Just PPFloat
unifyTypes _ _           = Nothing

unifyDomains :: PPDomain -> PPDomain -> PPDomain
unifyDomains Public Public = Public
unifyDomains Unknown x     = x
unifyDomains x Unknown     = x
unifyDomains _ _           = Private

