{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Privalog.Types 
  ( PPType(..), PPDomain(..)
  , unifyTypes
  , unifyDomains
  ) where

import Relude hiding (show, succ)

import Data.Text.Prettyprint.Doc
import Data.Data

import Text.Show

data PPType
  = PPBool
  | PPInt8 | PPInt16 | PPInt32 | PPInt64
  | PPUInt8 | PPUInt16 | PPUInt32 | PPUInt64
  | PPXorUInt8 | PPXorUInt16 | PPXorUInt32 | PPXorUInt64
  | PPFloat32 | PPFloat64
  | PPStr
  | PPAuto
  deriving (Ord, Eq, Data, Typeable)

data PPDomain
  = Public
  | Private
  | Unknown
  deriving (Ord, Eq, Data, Typeable)

instance Show PPType where
  show PPBool      = "bool"
  show PPInt8      = "int8"
  show PPInt16     = "int16"
  show PPInt32     = "int32"
  show PPInt64     = "int64"
  show PPUInt8     = "uint8"
  show PPUInt16    = "uint16"
  show PPUInt32    = "uint32"
  show PPUInt64    = "uint64"
  show PPXorUInt8  = "xor_uint8"
  show PPXorUInt16 = "xor_uint16"
  show PPXorUInt32 = "xor_uint32"
  show PPXorUInt64 = "xor_uint64"
  show PPStr       = "string"
  show PPFloat32   = "float32"
  show PPFloat64   = "float64"
  show PPAuto      = "auto"

instance Pretty PPType where
  pretty = pretty . show

instance Show PPDomain where
  show Public  = "public"
  show Private = "private"
  show Unknown = "unknown"

instance Pretty PPDomain where
  pretty = pretty . show

unifyTypes :: PPType -> PPType -> Maybe PPType
unifyTypes x y = unifyTypes' x y <|> (succ x >>= flip unifyTypes y)

unifyTypes' :: PPType -> PPType -> Maybe PPType
unifyTypes' x y 
  | x == y    = Just x
  | otherwise = succ y >>= unifyTypes' x

unifyDomains :: PPDomain -> PPDomain -> PPDomain
unifyDomains Public Public = Public
unifyDomains Unknown x     = x
unifyDomains x Unknown     = x
unifyDomains _ _           = Private

succ :: PPType -> Maybe PPType
succ PPBool      = Just PPInt8
succ PPInt8      = Just PPInt16
succ PPInt16     = Just PPInt32
succ PPInt32     = Just PPInt64
succ PPInt64     = Just PPFloat32
succ PPFloat32   = Just PPFloat64
succ PPUInt8     = Just PPInt16
succ PPUInt16    = Just PPInt32
succ PPUInt32    = Just PPInt64
succ PPUInt64    = Just PPInt64
succ PPXorUInt8  = Just PPXorUInt16
succ PPXorUInt16 = Just PPXorUInt32
succ PPXorUInt32 = Just PPXorUInt64
succ _           = Nothing

