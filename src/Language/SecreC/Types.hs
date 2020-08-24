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
