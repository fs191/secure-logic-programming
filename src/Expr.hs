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
  , PPType(..), PPDomain(..)
  , isConstExpr, isLeaf
  , isVar
  , _Pred
  , constStr
  , constTrue, constFalse
  , var
  , predicate
  ) where

---------------------------------------------------------
---- Arithmetic and Boolean expressions
---------------------------------------------------------

import Control.Exception
import Control.Lens hiding (plate)

import Data.Data
import Data.Generics.Uniplate.Data ()
import Data.Text.Prettyprint.Doc

import Prelude hiding ((<>))

import Language.SecreC.Types

data EvaluationException a
  = NonConstantTerm Expr
  deriving (Show, Exception)

data Ann = Ann
  { _type   :: Maybe PPType
  , _domain :: Maybe PPDomain
  }
  deriving (Ord, Show, Eq, Data, Typeable)

empty :: Ann
empty = Ann Nothing Nothing

-- artihmetic expressions
-- associative operations are represented with lists
data Expr
  = ConstInt   Ann Int
  | ConstFloat Ann Float
  | ConstStr   Ann String
  | ConstBool  Ann Bool
  | Var  Ann String
  | Not  Ann Expr
  | Neg  Ann Expr
  | Div  Ann Expr Expr
  | Sub  Ann Expr Expr
  | LT   Ann Expr Expr
  | LE   Ann Expr Expr
  | EQ   Ann Expr Expr
  | GT   Ann Expr Expr
  | GE   Ann Expr Expr
  | Mul  Ann [Expr]
  | Add  Ann [Expr]
  | Min  Ann [Expr]
  | Max  Ann [Expr]
  | And  Ann [Expr]
  | Or   Ann [Expr]
  | Pred Ann String [Expr]
  deriving (Ord,Show,Eq,Data,Typeable)
makePrisms ''Expr

instance Pretty Expr where
  pretty (Var _ x)        = pretty x
  pretty (ConstInt _ x)   = pretty x
  pretty (ConstStr _ x)   = pretty x
  pretty (ConstBool _ x)  = pretty x
  pretty (Pred _ n args)  = pretty n <> (tupled $ pretty <$> args)

--------------------------
-- is the expression constant (i.e. does not contain any variables)?
isConstExpr :: Expr -> Bool
isConstExpr _ = undefined

isLeaf :: Expr -> Bool
isLeaf (Var _ _)       = True
isLeaf (ConstBool _ _) = True
isLeaf (ConstInt _ _)  = True
isLeaf (ConstStr _ _)  = True
isLeaf _             = False

isVar :: Expr -> Bool
isVar (Var _ _) = True
isVar _       = False

constStr :: String -> Expr
constStr = ConstStr empty

constTrue :: Expr
constTrue = ConstBool empty True

constFalse :: Expr
constFalse = ConstBool empty False

var :: String -> Expr
var = Var empty

predicate :: String -> [Expr] -> Expr
predicate = Pred empty

