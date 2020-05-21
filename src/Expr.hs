{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable#-}

module Expr
  ( Expr(..)
  , BinOp(..), UnOp(..)
  , extractAllPredicates
  , isConstExpr
  , ruleIndent
  ) where

---------------------------------------------------------
---- Arithmetic and Boolean expressions
---------------------------------------------------------

import Prelude hiding ((<>))
import Control.Exception
import Data.Typeable
import Data.Foldable

import Data.Text.Prettyprint.Doc

data EvaluationException a
  = NonConstantTerm (Expr a)
  deriving (Show, Exception)

-- artihmetic expressions
data Expr a
  = Var a
  | ConstNum Int
  | ConstStr String
  | ConstBool Bool
  | Unary  UnOp   (Expr a)
  | Binary BinOp  (Expr a) (Expr a)
  | Pred   String [Expr a]
  deriving (Ord,Eq,Functor,Foldable,Show)

instance (Pretty a) => Pretty (Expr a) where
  pretty (Var x) = pretty x
  pretty (ConstNum x) = pretty x
  pretty (ConstStr x) = pretty x
  pretty (Unary op x) = pretty op <> pretty x
  pretty (Binary op x y) = pretty x <> pretty op <> pretty y

data BinOp
  = Div | Mult | Add | Sub
  | Min | Max
  | And | Or
  | Implies
  | BLT | BLE | BEQ | BGE | BGT | BAsgn
  deriving (Ord,Eq,Show)

instance Pretty UnOp where
  pretty = pretty . show

instance Pretty BinOp where
  pretty And = ",\n"
  pretty Or  = " OR "
  pretty x   = pretty . show $ x
  pretty BLT   = " < "
  pretty BLE   = " <= "
  pretty BEQ   = " == "
  pretty BGE   = " >= "
  pretty BGT   = " > "
  pretty BAsgn = " := "

data UnOp
  = Not
  | Neg
  deriving (Ord,Eq)

instance Show UnOp where
  show Not = "-"

------------------------------------------------------------------------------------
-- this is currently used only for visual feedback
ruleIndent :: String
ruleIndent = "  "
--------------------------
-- is the expression constant (i.e. does not contain any variables)?
isConstExpr :: Expr a -> Bool
isConstExpr expr = not . null $ toList expr

--------------------------
-- evaluate an expression
evalInt :: Expr a -> Maybe Int
evalInt (ConstNum x)    = Just x
evalInt (Binary op x y) = toBinIntOp op <*> evalInt x <*> evalInt y
evalInt (Unary op x)    = toUnIntOp op <*> evalInt x
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
evalBool (Binary op x y) = asum
  [ toBinBoolOp op <*> evalBool x <*> evalBool y
  , toBinPredOp op <*> evalInt  x <*> evalInt  y
  ]

toBinBoolOp :: BinOp -> Maybe (Bool -> Bool -> Bool)
toBinBoolOp And     = Just (&&)
toBinBoolOp Or      = Just (||)
toBinBoolOp Implies = Just $ \x y -> not y || x
toBinBoolOp _       = Nothing

toBinPredOp :: BinOp -> Maybe (Int -> Int -> Bool)
toBinPredOp BGT = Just (>)
toBinPredOp BLT = Just (<)
toBinPredOp BGE = Just (>=)
toBinPredOp BLE = Just (<=)
toBinPredOp BEQ = Just (==)
toBinPredOp _   = Nothing

extractAllPredicates :: Expr a -> [(String, [Expr a])]
extractAllPredicates bexpr =
    case bexpr of
        Pred f xs -> [(f,xs)]
        Unary _ x      -> processRec x
        Binary _ x1 x2 -> (processRec x1) ++ (processRec x2)
        _               -> []
    where processRec x = extractAllPredicates x

