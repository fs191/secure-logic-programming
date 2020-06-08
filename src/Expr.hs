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
  , Ann
  , isConstExpr, isLeaf
  , isVar
  , _Pred
  , constStr
  , constInt
  , constBool
  , constTrue, constFalse
  , var
  , predicate
  , less, lessEqual
  , greater, greaterEqual
  , equal
  , eNeg
  , eAdd, eSub
  , eInv
  , eMul, eDiv
  , eMin, eMax
  , eAnd, eOr
  , annLens
  , annType, domain
  , dbCol
  , _Var
  , _ConstStr
  , _DBCol
  ) where

---------------------------------------------------------
---- Arithmetic and Boolean expressions
---------------------------------------------------------

import Control.Exception
import Control.Lens hiding (children)

import Data.Generics.Uniplate ()
import Data.Data
import Data.Data.Lens
import Data.Text.Prettyprint.Doc

import Language.SecreC.Types

data Ann = Ann
  { 
    -- Datatype of the term
    _annType :: Maybe PPType
    -- Security domain of the term
  , _domain  :: Maybe PPDomain
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
  | DBCol      Ann String
  | Var  Ann String
  | Not  Ann Expr
  | Neg  Ann Expr
  | Inv  Ann Expr
  | Div  Ann Expr Expr
  | Sub  Ann Expr Expr
  | Lt   Ann Expr Expr
  | Le   Ann Expr Expr
  | Eq   Ann Expr Expr
  | Gt   Ann Expr Expr
  | Ge   Ann Expr Expr
  | Mul  Ann Expr Expr
  | Add  Ann Expr Expr
  | Min  Ann Expr Expr
  | Max  Ann Expr Expr
  | And  Ann Expr Expr
  | Or   Ann Expr Expr
  | Pred Ann String [Expr]
  deriving (Ord,Show,Eq,Data,Typeable)
makeLenses ''Ann
makePrisms ''Ann
makePrisms ''Expr

instance Pretty Expr where
  pretty e@(Var _ x)      = pretty x <> prettyType e
  pretty (ConstInt _ x)   = pretty x
  pretty e@(ConstStr _ x) = pretty x <> prettyType e
  pretty e@(DBCol _ x)    = pretty x <> prettyType e
  pretty (ConstBool _ x)  = pretty x
  pretty (Pred _ n args)  = pretty n <> (tupled $ pretty <$> args)
  pretty (Not _ e)        = "!" <> pretty e
  pretty (Neg _ e)        = "-" <> pretty e
  pretty (Inv _ e)        = "(" <> pretty e <> ")^(-1)"
  pretty (Div _ x y)      = pretty x <+> "/" <+> pretty y
  pretty (Sub _ x y)      = pretty x <+> "-" <+> pretty y
  pretty (Lt _ x y)       = pretty x <+> "<" <+> pretty y
  pretty (Le _ x y)       = pretty x <+> "=<" <+> pretty y
  pretty (Eq _ x y)       = pretty x <+> "=" <+> pretty y
  pretty (Gt _ x y)       = pretty x <+> ">" <+> pretty y
  pretty (Ge _ x y)       = pretty x <+> ">=" <+> pretty y
  pretty (Mul _ x y)      = pretty x <+> "*" <+> pretty y
  pretty (Add _ x y)      = pretty x <+> "+" <+> pretty y
  pretty (Or _ x y)       = pretty x <> ";\n" <> pretty y
  pretty (And _ x y)      = pretty x <> ",\n" <> pretty y

data EvaluationException a
  = NonConstantTerm Expr
  deriving (Show, Exception)

prettyType :: Expr -> Doc ann
prettyType e 
  | t == Just PPAuto &&
    d == Just Public ||
    t == Nothing     ||
    d == Nothing 
      = ""
  | otherwise = " :" <+> pretty d <+> pretty t
  where (Ann t d) = head $ e ^. partsOf annLens

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

constInt :: Int -> Expr
constInt = ConstInt empty

constTrue :: Expr
constTrue = ConstBool empty True

constFalse :: Expr
constFalse = ConstBool empty False

constBool :: Bool -> Expr
constBool = ConstBool empty

var :: String -> Expr
var = Var empty

predicate :: String -> [Expr] -> Expr
predicate = Pred empty

less :: Expr -> Expr -> Expr
less = Lt empty

lessEqual :: Expr -> Expr -> Expr
lessEqual = Le empty

greater :: Expr -> Expr -> Expr
greater = Gt empty

greaterEqual :: Expr -> Expr -> Expr
greaterEqual = Ge empty

equal :: Expr -> Expr -> Expr
equal = Eq empty

eNeg :: Expr -> Expr
eNeg = Neg empty

eMax :: Expr -> Expr -> Expr
eMax = Max empty

eMin :: Expr -> Expr -> Expr
eMin = Min empty 

eMul :: Expr -> Expr -> Expr
eMul = Mul empty

eDiv :: Expr -> Expr -> Expr
eDiv = Div empty

eAdd :: Expr -> Expr -> Expr
eAdd = Add empty

eSub :: Expr -> Expr -> Expr
eSub = Sub empty

eInv :: Expr -> Expr
eInv = Inv empty

eAnd :: Expr -> Expr -> Expr
eAnd = And empty

eOr :: Expr -> Expr -> Expr
eOr = Or empty

dbCol :: String -> Expr
dbCol = DBCol empty

-- | A traversal for accessing the annotation of a term
annLens :: Traversal' Expr Ann
annLens = template

