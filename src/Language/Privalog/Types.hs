{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Privalog.Types 
  ( PPType(..), PPDomain(..)
  , unifyTypes
  , unifyDomains
  ) where

import Relude hiding (show)

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

isNumeric :: PPType -> Bool
isNumeric PPStr = False
isNumeric _     = True

typeSizeBytes :: PPType -> Int
typeSizeBytes PPBool = 1
typeSizeBytes PPInt8 = 1
typeSizeBytes PPUInt8 = 1
typeSizeBytes PPXorUInt8 = 1
typeSizeBytes PPInt16 = 2
typeSizeBytes PPUInt16 = 2
typeSizeBytes PPXorUInt16 = 2
typeSizeBytes PPInt32 = 4
typeSizeBytes PPUInt32 = 4
typeSizeBytes PPXorUInt32 = 4
typeSizeBytes PPFloat32 = 4
typeSizeBytes PPInt64 = 8
typeSizeBytes PPUInt64 = 8
typeSizeBytes PPXorUInt64 = 8
typeSizeBytes PPFloat64 = 8

unifyTypes :: PPType -> PPType -> Maybe PPType
unifyTypes x y | x == y  = Just x
unifyTypes PPAuto x      = Just x
unifyTypes x PPAuto      = Just x
unifyTypes PPFloat32 PPInt32 = Just PPFloat32
unifyTypes PPInt32 PPFloat32 = Just PPFloat32
unifyTypes _ _           = Nothing

unifyDomains :: PPDomain -> PPDomain -> PPDomain
unifyDomains Public Public = Public
unifyDomains Unknown x     = x
unifyDomains x Unknown     = x
unifyDomains _ _           = Private

