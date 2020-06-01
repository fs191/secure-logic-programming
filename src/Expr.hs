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
  , isVar
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
  | BLT | BLE | BEQ | BGE | BGT
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
  pretty And   = ",\n"
  pretty Or    = " ; "
  pretty BLT   = " < "
  pretty BLE   = " <= "
  pretty BEQ   = " = "
  pretty BGE   = " >= "
  pretty BGT   = " > "
  pretty Add   = " + "
  pretty x     = pretty $ show x

--------------------------
-- is the expression constant (i.e. does not contain any variables)?
isConstExpr :: Expr a -> Bool
isConstExpr expr = not . null $ toList expr

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

isVar :: Expr a -> Bool
isVar (Var _) = True
isVar _       = False

