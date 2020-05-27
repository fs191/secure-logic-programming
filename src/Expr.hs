{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr
  ( Expr(..)
  , BinOp(..), UnOp(..)
  , extractAllPredicates
  , isConstExpr, isLeaf
  , simplifyBool
  , _Pred
  ) where

---------------------------------------------------------
---- Arithmetic and Boolean expressions
---------------------------------------------------------

import Control.Exception
import Control.Lens hiding (plate)

import Data.Data
import Data.Foldable
import Data.Generics.Uniplate.Data ()
import Data.Text.Prettyprint.Doc

import Prelude hiding ((<>))

data EvaluationException a
  = NonConstantTerm (Expr a)
  deriving (Show, Exception)

data UnOp
  = Not
  | Neg
  deriving (Ord,Eq,Data, Typeable)

instance Show UnOp where
  show Not = "-"

data BinOp
  = Div | Mult | Add | Sub
  | Min | Max
  | And | Or
  | BLT | BLE | BEQ | BGE | BGT | BAsgn
  deriving (Ord,Eq,Show,Data,Typeable)

-- artihmetic expressions
data Expr a
  = Var a
  | ConstNum Int
  | ConstStr String
  | ConstBool Bool
  | Unary  UnOp   (Expr a)
  | Binary BinOp  (Expr a) (Expr a)
  | Pred   String [Expr a]
  deriving (Ord,Functor,Foldable,Show,Eq,Data,Typeable)
makePrisms ''Expr

instance (Pretty a) => Pretty (Expr a) where
  pretty (Var x)        = pretty x
  pretty (ConstNum x)   = pretty x
  pretty (ConstStr x)   = pretty x
  pretty (ConstBool x)  = pretty x
  pretty (Unary o x)    = pretty o <> pretty x
  pretty (Binary o x y) = pretty x <> pretty o <> pretty y
  pretty (Pred n args)  = pretty n <> (tupled $ pretty <$> args)

instance Pretty UnOp where
  pretty = pretty . show

instance Pretty BinOp where
  pretty And = ",\n"
  pretty Or  = " OR "
  pretty BLT   = " < "
  pretty BLE   = " <= "
  pretty BEQ   = " = "
  pretty BGE   = " >= "
  pretty BGT   = " > "
  pretty BAsgn = " := "
  pretty Add   = " + "
  pretty x     = pretty $ show x

--------------------------
-- is the expression constant (i.e. does not contain any variables)?
isConstExpr :: Expr a -> Bool
isConstExpr expr = not . null $ toList expr

--------------------------
-- evaluate an expression
evalInt :: Expr a -> Maybe Int
evalInt (ConstNum x)    = Just x
evalInt (Binary o x y) = toBinIntOp o <*> evalInt x <*> evalInt y
evalInt (Unary o x)    = toUnIntOp o <*> evalInt x
evalInt _               = Nothing

toBinIntOp :: BinOp -> Maybe (Int -> Int -> Int)
toBinIntOp Add  = Just (+)
toBinIntOp Mult = Just (*)
toBinIntOp Sub  = Just (-)
toBinIntOp _    = Nothing

toUnIntOp :: UnOp -> Maybe (Int -> Int)
toUnIntOp Neg = Just negate
toUnIntOp _   = Nothing

evalBool :: Expr a -> Maybe Bool
evalBool (ConstBool x) = Just x
evalBool (Binary o x y) = asum
  [ toBinBoolOp o <*> evalBool x <*> evalBool y
  , toBinPredOp o <*> evalInt  x <*> evalInt  y
  ]

toBinBoolOp :: BinOp -> Maybe (Bool -> Bool -> Bool)
toBinBoolOp And     = Just (&&)
toBinBoolOp Or      = Just (||)
toBinBoolOp _       = Nothing

toBinPredOp :: BinOp -> Maybe (Int -> Int -> Bool)
toBinPredOp BGT = Just (>)
toBinPredOp BLT = Just (<)
toBinPredOp BGE = Just (>=)
toBinPredOp BLE = Just (<=)
toBinPredOp BEQ = Just (==)
toBinPredOp _   = Nothing

simplifyBool :: Expr a -> Expr a
simplifyBool = id

extractAllPredicates :: Expr a -> [(String, [Expr a])]
extractAllPredicates bexpr =
    case bexpr of
        Pred f xs -> [(f,xs)]
        Unary _ x      -> processRec x
        Binary _ x1 x2 -> (processRec x1) ++ (processRec x2)
        _               -> []
    where processRec x = extractAllPredicates x

isLeaf :: Expr a -> Bool
isLeaf (Var _)       = True
isLeaf (ConstBool _) = True
isLeaf (ConstNum _)  = True
isLeaf (ConstStr _)  = True
isLeaf _             = False

