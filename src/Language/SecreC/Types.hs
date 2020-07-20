{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SecreC.Types 
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
  | PPAuto
  deriving (Ord, Show, Eq, Data, Typeable)

data PPDomain
  = Public
  | Private
  | Unknown
  deriving (Ord, Show, Eq, Data, Typeable)

instance Pretty PPType where
  pretty PPBool = "bool"
  pretty PPInt  = "int"
  pretty PPStr  = "string"
  pretty PPAuto = "auto"

instance Pretty PPDomain where
  pretty Public  = "public"
  pretty Private = "private"
  pretty Unknown = "unknown"

unifyTypes :: PPType -> PPType -> PPType
unifyTypes x y
  | x == PPAuto = y
  | y == PPAuto || x == y
                = x
  | otherwise   = error $ "Cannot unify types " ++ show x ++ " and " ++ show y

unifyDomains :: PPDomain -> PPDomain -> PPDomain
unifyDomains Public Public = Public
unifyDomains Unknown x     = x
unifyDomains x Unknown     = x
unifyDomains _ _           = Private
