{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Privalog.Types 
  ( PPType(..), PPDomain(..)
  , unifyTypes
  , unifyDomains
  ) where

import Relude

import Data.Text.Prettyprint.Doc
import Data.Data

import Text.Show as S

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
  pretty = pretty . S.show

instance Show PPDomain where
  show Public  = "public"
  show Private = "private"
  show Unknown = "unknown"

instance Pretty PPDomain where
  pretty = pretty . S.show

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

