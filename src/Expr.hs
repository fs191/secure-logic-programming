{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr
  ( Expr(..)
  , PPType(..), PPDomain(..)
  , Ann
  , isConstExpr, isLeaf
  , isVar
  , _Pred
  , predName
  , predArgs
  , constStr
  , constInt
  , constBool
  , constTrue, constFalse
  , var
  , predicate
  , attribute
  , less, lessEqual
  , greater, greaterEqual
  , equal
  , eIs
  , eNeg
  , eAdd, eSub
  , eInv
  , eMul, eDiv
  , eMin, eMax
  , eAnd, eOr
  , eList
  , ann
  , leftHand, rightHand
  , arg
  , annType, domain
  , applyTyping
  , getAnn
  , getVarName
  , _Var
  , _ConstStr
  , andsToList
  , predicateVarNames
  , predicateName
  , predicateArity
  , annotateWithBindings
  , identifier
  , unifyExprAnns
  , unifyExprTypes
  , unifyExprDomains
  ) where

---------------------------------------------------------
---- Arithmetic and Boolean expressions
---------------------------------------------------------

import Control.Exception
import Control.Lens hiding (transform, children, List)

import Data.Data
import Data.Generics.Uniplate.Data as U
import Data.List as L
import Data.Set as S hiding (empty)
import Data.Text.Prettyprint.Doc

import Language.SecreC.Types
import Language.Prolog.PrologSource

import Annotation

-- artihmetic expressions
-- associative operations are represented with lists
data Expr
  = ConstInt   {_ann :: !Ann, _intVal :: !Int}
  | ConstFloat {_ann :: !Ann, _floatVal :: !Float}
  | ConstStr   {_ann :: !Ann, _strVal :: !String}
  | ConstBool  {_ann :: !Ann, _boolVal :: !Bool}
  | Attribute  {_ann :: !Ann, _attrName :: !String}
  | Var  {_ann :: !Ann, _varName :: !String}
  | Not  {_ann :: !Ann, _arg :: !Expr}
  | Neg  {_ann :: !Ann, _arg :: !Expr}
  | Inv  {_ann :: !Ann, _arg :: !Expr}
  | Div  {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Sub  {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Lt   {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Le   {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Eq   {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Is   {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Gt   {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Ge   {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Mul  {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Add  {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Min  {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Max  {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | And  {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Or   {_ann :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Pred {_ann :: !Ann, _predName :: !String, _predArgs :: ![Expr]}
  | List {_ann :: !Ann, _vals :: ![Expr]}
  deriving (Ord,Show,Eq,Data,Typeable)
makeLenses ''Expr
makePrisms ''Expr

instance Pretty Expr where
  pretty (Var e x)        = pretty x <+> (pretty $ show e)
  pretty (ConstInt e x)   = pretty x <+> (pretty $ show e)
  pretty (ConstStr e x)   = pretty x <+> (pretty $ show e)
  pretty (ConstBool e x)  = pretty x <+> (pretty $ show e)
  pretty (ConstFloat e x) = pretty x <+> (pretty $ show e)
  pretty (Attribute e x)  = pretty x <+> (pretty $ show e)
  pretty (Pred e n args)  = pretty n <> tupled (pretty <$> args) <+> (pretty $ show e)
  pretty (Not e x)        = "!" <> pretty x <+> (pretty $ show e)
  pretty (Neg e x)        = "-(" <> pretty x <> ")" <+> (pretty $ show e)
  pretty (Inv e x)        = "(" <> pretty x <> ")^(-1)" <+> (pretty $ show e)
  pretty (Div e x y)      = pretty x <+> "/" <+> pretty y <+> (pretty $ show e)
  pretty (Sub e x y)      = pretty x <+> "-" <+> pretty y <+> (pretty $ show e)
  pretty (Lt e x y)       = pretty x <+> "<" <+> pretty y <+> (pretty $ show e)
  pretty (Le e x y)       = pretty x <+> "=<" <+> pretty y <+> (pretty $ show e)
  pretty (Eq e x y)       = pretty x <+> "=" <+> pretty y <+> (pretty $ show e)
  pretty (Is e x y)       = pretty x <+> "is" <+> pretty y <+> (pretty $ show e)
  pretty (Gt e x y)       = pretty x <+> ">" <+> pretty y <+> (pretty $ show e)
  pretty (Ge e x y)       = pretty x <+> ">=" <+> pretty y <+> (pretty $ show e)
  pretty (Mul e x y)      = pretty x <+> "*" <+> pretty y <+> (pretty $ show e)
  pretty (Add e x y)      = pretty x <+> "+" <+> pretty y <+> (pretty $ show e)
  pretty (Min e x y)      = "min(" <> pretty x <> ", " <> pretty y <> ")" <+> (pretty $ show e)
  pretty (Max e x y)      = "max(" <> pretty x <> ", " <> pretty y <> ")" <+> (pretty $ show e)
  pretty (Or e x y)       = pretty x <> ";\n" <> pretty y <+> (pretty $ show e)
  pretty (And _ x y)      = pretty x <> ",\n" <> pretty y
  pretty (List e x)       = (list $ pretty <$> x) <+> (pretty $ show e)

instance PrologSource Expr where
  prolog (Var _ x)        = pretty x
  prolog (ConstInt _ x)   = pretty x
  prolog (ConstStr _ x)   = pretty x
  prolog (ConstBool _ x)  = pretty x
  prolog (ConstFloat _ x) = pretty x
  prolog (Attribute _ x)  = pretty x
  prolog (Pred _ n args)  = pretty n <> tupled (prolog <$> args) -- <+> (prolog $ show e)
  prolog (Not _ e)        = "!" <> prolog e
  prolog (Neg _ e)        = "-" <> prolog e
  prolog (Inv _ e)        = "(" <> prolog e <> ")^(-1)"
  prolog (Div _ x y)      = prolog x <+> "/" <+> prolog y
  prolog (Sub _ x y)      = prolog x <+> "-" <+> prolog y
  prolog (Lt _ x y)       = prolog x <+> "<" <+> prolog y
  prolog (Le _ x y)       = prolog x <+> "=<" <+> prolog y
  prolog (Eq _ x y)       = prolog x <+> "=" <+> prolog y
  prolog (Is _ x y)       = prolog x <+> "is" <+> prolog y
  prolog (Gt _ x y)       = prolog x <+> ">" <+> prolog y
  prolog (Ge _ x y)       = prolog x <+> ">=" <+> prolog y
  prolog (Mul _ x y)      = prolog x <+> "*" <+> prolog y
  prolog (Add _ x y)      = prolog x <+> "+" <+> prolog y
  prolog (Min _ x y)      = "min(" <> prolog x <> ", " <> prolog y <> ")"
  prolog (Max _ x y)      = "max(" <> prolog x <> ", " <> prolog y <> ")"
  prolog (Or _ x y)       = prolog x <> ";\n" <> prolog y
  prolog (And _ x y)      = prolog x <> ",\n" <> prolog y
  prolog (List _ x)       = list $ prolog <$> x

data EvaluationException a
  = NonConstantTerm !Expr
  deriving (Show, Exception)


--------------------------
-- is the expression constant (i.e. does not contain any variables)?
isConstExpr :: Expr -> Bool
isConstExpr (Var _ _) = False
isConstExpr _         = True

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
constStr = ConstStr a
  where
    a = empty & annBound .~ True
              & annType  .~ PPStr
              & domain   .~ Public

constInt :: Int -> Expr
constInt = ConstInt a
  where
    a = empty & annBound .~ True
                & annType  .~ PPInt
                & domain   .~ Public


constTrue :: Expr
constTrue = ConstBool empty True

constFalse :: Expr
constFalse = ConstBool empty False

constBool :: Bool -> Expr
constBool = ConstBool (empty & annBound .~ True
                             & annType  .~ PPBool
                             & domain   .~ Public)

var :: String -> Expr
var n = Var a n 
  where a = empty & annType .~ PPAuto
                    & domain  .~ Unknown

predicate :: String -> [Expr] -> Expr
predicate = Pred a
  where
    a = empty & annBound .~ True
                & annType  .~ PPBool
                & domain   .~ Unknown

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

eList :: [Expr] -> Expr
eList = List empty

eIs :: Expr -> Expr -> Expr
eIs = Is empty

getAnn :: Expr -> Ann
getAnn x = head $ x ^.. ann

-- | Turns all `And`s to a list, 
-- starting from the root of the expression
andsToList :: Expr -> [Expr]
andsToList (And _ l r) = andsToList l <> andsToList r
andsToList x = [x]

predicateName :: Expr -> String
predicateName (Pred _ p _) = p
predicateName _ = error "Expecting a predicate"

predicateVarNames :: Expr -> [String]
predicateVarNames (Pred _ _ vs) = [n | (Var _ n) <- vs]
predicateVarNames _ = error "Expecting a predicate"

predicateArity :: Expr -> Int
predicateArity (Pred _ _ xs) = length xs
predicateArity _ = error "Expecting a predicate"

getVarName :: Expr -> String
getVarName (Var _ n) = n
getVarName _ = error "Expecting a variable"

annotateWithBindings :: Set String -> Expr -> Expr
annotateWithBindings s = L.foldl1 eAnd
                       . annotateWithBindings' s 
                       . andsToList

annotateWithBindings' :: Set String -> [Expr] -> [Expr]
annotateWithBindings' _ [] = []
annotateWithBindings' s (e:et) = e' : annotateWithBindings' s' et
  where
    f v@(Var a n) 
      | n `S.member` s = Var (a & annBound .~ True) n
      | otherwise      = v
    f x = x
    e' = U.transform f e
    s' = fromList [n | (Var _ n) <- U.universe e] <> s

identifier :: Expr -> Maybe String
identifier (Var _ n)       = Just n
identifier (ConstStr _ n)  = Just n
identifier (Pred _ n _)    = Just n
identifier (Attribute _ n) = Just n
identifier _ = Nothing

attribute :: String -> Expr
attribute = Attribute empty

unifyExprAnns :: Expr -> Expr -> Expr
unifyExprAnns x y = x & ann %~ (unifyAnns a)
  where a = y ^. ann

unifyExprTypes :: Expr -> Expr -> PPType
unifyExprTypes x y = unifyTypes xd yd
  where
    xd = head $ x ^.. ann . annType
    yd = head $ y ^.. ann . annType

unifyExprDomains :: Expr -> Expr -> PPDomain
unifyExprDomains x y = unifyDomains xd yd
  where
    xd = head $ x ^.. ann . domain
    yd = head $ y ^.. ann . domain

applyTyping :: Typing -> Expr -> Expr
applyTyping t e = e & ann . typing %~ unifyTypings t

