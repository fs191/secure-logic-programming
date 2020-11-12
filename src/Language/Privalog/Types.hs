{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Privalog.Types 
  ( PPType(..), PPDomain(..)
  , unifyTypes
  , unifyDomains
  ) where

import Relude hiding (show, widen)

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
unifyTypes x y = unifyTypes' x y <|> (widen x >>= flip unifyTypes y)
  where
    unifyTypes' x' y'
      | x' == y'  = Just x'
      | otherwise = widen y' >>= unifyTypes' x'

widen :: PPType -> Maybe PPType
widen PPBool      = Just PPInt8
widen PPInt8      = Just PPInt16
widen PPInt16     = Just PPInt32
widen PPInt32     = Just PPInt64
widen PPInt64     = Just PPFloat32
widen PPFloat32   = Just PPFloat64
widen PPUInt8     = Just PPInt16
widen PPUInt16    = Just PPInt32
widen PPUInt32    = Just PPInt64
widen PPUInt64    = Just PPInt64
widen PPXorUInt8  = Just PPXorUInt16
widen PPXorUInt16 = Just PPXorUInt32
widen PPXorUInt32 = Just PPXorUInt64
widen _           = Nothing

unifyDomains :: PPDomain -> PPDomain -> PPDomain
unifyDomains Public Public = Public
unifyDomains Unknown x     = x
unifyDomains x Unknown     = x
unifyDomains _ _           = Private

