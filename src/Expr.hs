{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains the Datalog expression tree representation and
-- helper functions for accessing parts of the expression tree
module Expr
  ( Expr(..)
  , Aggregation(..)
  , PPType(..), PPDomain(..)
  , Ann
  , isLeaf
  , isVar
  , _Pred
  , predName
  , predArgs
  , constStr
  , constInt
  , constFloat
  , constBool
  , constTrue, constFalse
  , var
  , predicate
  , attribute
  , less, lessEqual
  , greater, greaterEqual
  , equal
  , eIs, eUn
  , eNeg
  , eNot
  , eInv
  , eSqrt, ePow
  , eAdd, eSub
  , eMul, eDiv, eFDiv
  , eAnd, eOr
  , eList
  , eNeq
  , eTrue
  , eFalse
  , isConstExpr
  , varNames
  , annotation
  , leftHand, rightHand
  , aggrName, aggrPred, sourceExpr, targetExpr
  , arg
  , annType, domain
  , applyTyping
  , _Var
  , _ConstStr
  , andsToList
  , predicateVarNames
  , predicateBoundedVarNames
  , predicateFreeVarNames
  , predicateVarNames
  , predicateVars
  , predicateArity
  , annotateWithBindings
  , identifier
  , hole
  , unifyExprAnns, unifyExprAnnsWithError
  , unifyExprTypes, unifyExprTypesWithError
  , unifyExprDomains
  , isArithmetic, isPredicative
  , applyAnnotation
  ) where

---------------------------------------------------------
---- Arithmetic and Boolean expressions
---------------------------------------------------------

import Control.Lens hiding (transform, children, List)

import Data.Data
import Data.Generics.Uniplate.Data as U
import Data.List as L
import Data.Maybe
import Data.Set as S hiding (empty)
import Data.Text.Prettyprint.Doc

import Language.Privalog.Types
import Language.Prolog.PrologSource

import Annotation

-- | A datatype for representing datalog expression trees.
data Expr
  = ConstInt   {_annotation :: !Ann, _intVal :: !Int}
  | ConstFloat {_annotation :: !Ann, _floatVal :: !Float}
  | ConstStr   {_annotation :: !Ann, _strVal :: !String}
  | ConstBool  {_annotation :: !Ann, _boolVal :: !Bool}
  | Attribute  {_annotation :: !Ann, _attrName :: !String}
  | Var  {_annotation :: !Ann, _varName :: !String}
  | Not  {_annotation :: !Ann, _arg :: !Expr}
  | Neg  {_annotation :: !Ann, _arg :: !Expr}
  | Inv  {_annotation :: !Ann, _arg :: !Expr}
  | Sqrt {_annotation :: !Ann, _arg :: !Expr}
  | FDiv {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Div  {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Mod  {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Sub  {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Lt   {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Le   {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Eq   {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Un   {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Is   {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Neq  {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Gt   {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Ge   {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Mul  {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Add  {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Pow  {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | And  {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Or   {_annotation :: !Ann, _leftHand :: !Expr, _rightHand :: !Expr}
  | Pred {_annotation :: !Ann, _predName :: !String, _predArgs :: ![Expr]}
  | List {_annotation :: !Ann, _vals :: ![Expr]}
  | Hole {_annotation :: !Ann}
  | Aggr {_annotation :: !Ann, _aggrName :: !String, _aggrPred :: !Expr, _sourceExpr :: !Expr, _targetExpr :: !Expr}
  deriving (Ord,Show,Eq,Data,Typeable)
makeLenses ''Expr
makePrisms ''Expr

instance Pretty Expr where
  pretty (Var e x)        = pretty x <+> pretty e
  pretty (ConstInt e x)   = pretty x <+> pretty e
  pretty (ConstStr e x)   = pretty x <+> pretty e
  pretty (ConstBool e x)  = pretty x <+> pretty e
  pretty (ConstFloat e x) = pretty x <+> pretty e
  pretty (Attribute e x)  = pretty x <+> pretty e
  pretty (Hole e)         = "_" <+> pretty e
  pretty (Pred e n args)  = pretty n <> tupled (pretty <$> args) <+> pretty e
  pretty (Not e x)        = "!" <> pretty x <+> pretty e
  pretty (Neg e x)        = "-(" <> pretty x <> ")" <+> pretty e
  pretty (Inv e x)        = "(" <> pretty x <> ")^(-1)"    <+> pretty e
  pretty (Div e x y)      = pretty x <+> "/" <+> pretty y  <+> pretty e
  pretty (FDiv e x y)     = pretty x <+> "/" <+> pretty y  <+> pretty e
  pretty (Sub e x y)      = pretty x <+> "-" <+> pretty y  <+> pretty e
  pretty (Lt e x y)       = pretty x <+> "<" <+> pretty y  <+> pretty e
  pretty (Le e x y)       = pretty x <+> "=<" <+> pretty y <+> pretty e
  pretty (Eq e x y)       = pretty x <+> "=:=" <+> pretty y  <+> pretty e
  pretty (Un e x y)       = pretty x <+> "=" <+> pretty y <+> pretty e
  pretty (Is e x y)       = pretty x <+> "is" <+> pretty y <+> pretty e
  pretty (Neq _ x y)       = pretty x <+> "=\\=" <+> pretty y
  pretty (Gt e x y)       = pretty x <+> ">" <+> pretty y  <+> pretty e
  pretty (Ge e x y)       = pretty x <+> ">=" <+> pretty y <+> pretty e
  pretty (Mul e x y)      = pretty x <+> "*" <+> pretty y  <+> pretty e
  pretty (Add e x y)      = pretty x <+> "+" <+> pretty y  <+> pretty e
  pretty (Or e x y)       = pretty x <> ";\n" <> pretty y  <+> pretty e
  pretty (And _ x y)      = pretty x <> ",\n" <> pretty y
  pretty (List e x)       = (list $ pretty <$> x) <+> pretty e
  pretty (Pow e x y)      = pretty x <> "^" <> pretty y <+> pretty e
  pretty (Aggr e f p x y) = pretty f <> "(" <> pretty p <> ", " <> pretty x <> "," <> pretty y <> ")" <+> pretty e
  pretty (Sqrt e x)       = "sqrt(" <> pretty x <> ")" <+> pretty e
  pretty x                = error $ "pretty not defined for " ++ show x

instance PrologSource Expr where
  prolog (Var _ x)        = pretty x
  prolog (ConstInt _ x)   = pretty x
  prolog (ConstStr _ x)   = pretty x
  prolog (ConstBool _ x)  = pretty x
  prolog (ConstFloat _ x) = pretty x
  prolog (Attribute _ x)  = pretty x
  prolog (Hole _)         = "_"
  prolog (Pred _ n args)  = pretty n <> tupled (prolog <$> args) -- <+> (prolog $ show e)
  prolog (Not _ e)        = "\\+(" <> prolog e <> ")"
  prolog (Neg _ e)        = "-(" <> prolog e <> ")"
  prolog (Inv _ e)        = "(" <> prolog e <> ")^(-1)"
  prolog (Sqrt _ e)       = "sqrt(" <> prolog e <> ")"
  prolog (Div _ x y)      = "div(" <> prolog x <> ", " <> prolog y <> ")"
  prolog (Mod _ x y)      = "mod(" <> prolog x <> ", " <> prolog y <> ")"
  prolog (FDiv _ x y)     = "(" <> prolog x <> ")/(" <> prolog y <> ")"
  prolog (Sub _ x y)      = "(" <> prolog x <> ")-(" <> prolog y <> ")"
  prolog (Lt _ x y)       = "(" <> prolog x <> ")<(" <> prolog y <> ")"
  prolog (Le _ x y)       = "(" <> prolog x <> ")=<(" <> prolog y <> ")"
  prolog (Eq _ x y)       = "(" <> prolog x <> ")=:=(" <> prolog y <> ")"
  prolog (Un _ x y)       = "(" <> prolog x <> ")=(" <> prolog y <> ")"
  prolog (Is _ x y)       = "(" <> prolog x <> ")is(" <> prolog y <> ")"
  prolog (Neq _ x y)      = "(" <> prolog x <> ")=\\=(" <> prolog y <> ")"
  prolog (Gt _ x y)       = "(" <> prolog x <> ")>(" <> prolog y <> ")"
  prolog (Ge _ x y)       = "(" <> prolog x <> ")>=(" <> prolog y <> ")"
  prolog (Mul _ x y)      = "(" <> prolog x <> ")*(" <> prolog y <> ")"
  prolog (Add _ x y)      = "(" <> prolog x <> ")+(" <> prolog y <> ")"
  prolog (Or _ x y)       = prolog x <> ";\n" <> prolog y
  prolog (And _ x y)      = prolog x <> ",\n" <> prolog y
  prolog (List _ x)       = list $ prolog <$> x
  prolog (Pow _ x y)      = "(" <> prolog x <> ")^(" <> prolog y <> ")"
  -- TODO there may be more compact/better ways for aggregations
  prolog (Aggr _ "times" p x y) = "findall(" <> "X0" <> "," <> "(" <> prolog p <> "," <> "X0 is log(" <> prolog x <> ")), Xs)" <> "," <+>
                                  "sum_list(Xs,Y0)" <> "," <+>
                                  prolog y <+> "is" <+> "exp(" <> "Y0" <> ")"
  prolog (Aggr _ "avg" p x y) = vsep
                                  [
                                    "findall" <> tupled
                                      [ prolog x
                                      , prolog p
                                      , "Xs"
                                      ]
                                  , "sum_list(Xs,Y0)"
                                  , "length(Xs,N)"
                                  , prolog y <+> "is Y0/N"
                                  ]
  prolog (Aggr _ f p x y) = "findall(" <> prolog x <> "," <> prolog p <> ", Xs)," <+>
                            pretty (prologAggr f) <> "_list(Xs," <> prolog y <> ")"
  prolog x = error $ show x

data Aggregation
  = Min
  | Max
  | Sum
  | Count
  | Average
  deriving (Show, Eq, Data, Typeable)

-- map datalog aggregations to prolog aggregations
prologAggr :: String -> String
prologAggr "sum"   = "sum_list"
prologAggr "min"   = "min_list"
prologAggr "max"   = "max_list"
prologAggr "count" = "length"

isLeaf :: Expr -> Bool
isLeaf (Var _ _)       = True
isLeaf (ConstBool _ _) = True
isLeaf (ConstInt _ _)  = True
isLeaf (ConstStr _ _)  = True
isLeaf _             = False

isVar :: Expr -> Bool
isVar (Var _ _) = True
isVar _         = False

-- | Creates a new constant string
constStr :: String -> Expr
constStr = ConstStr a
  where
    a = empty & annBound .~ True
              & annType  .~ PPStr
              & domain   .~ Public

-- | Creates a new constant integer
constInt :: Int -> Expr
constInt = ConstInt a
  where
    a = empty & annBound .~ True
              & annType  .~ PPInt
              & domain   .~ Public

constFloat :: Float -> Expr
constFloat = ConstFloat a
  where
    a = empty & annBound .~ True
              & annType  .~ PPFloat
              & domain   .~ Public

-- | Creates a new true boolean
constTrue :: Expr
constTrue = ConstBool empty True

-- | Creates a new false boolean
constFalse :: Expr
constFalse = ConstBool empty False

-- | Creates a new constant boolean
constBool :: Bool -> Expr
constBool = ConstBool (empty & annBound .~ True
                             & annType  .~ PPBool
                             & domain   .~ Public)

-- | Creates a new variable
var :: String -> Expr
var n = Var a n 
  where a = empty & annType  .~ PPAuto
                  & domain   .~ Unknown
                  & annBound .~ False

-- | Creates a new predicate from name and arguments
predicate :: String -> [Expr] -> Expr
predicate = Pred a
  where
    a = empty & annBound .~ True
              & annType  .~ PPBool
              & domain   .~ Unknown

-- | Creates a new less-than expression
less :: Expr -> Expr -> Expr
less = Lt empty

-- | Creates a new less-than-or-equal expression
lessEqual :: Expr -> Expr -> Expr
lessEqual = Le empty

-- | Creates a new greater-than expression
greater :: Expr -> Expr -> Expr
greater = Gt empty

-- | Creates a new greater-than-or-equal expression
greaterEqual :: Expr -> Expr -> Expr
greaterEqual = Ge empty

-- | Creates a new equals expression
equal :: Expr -> Expr -> Expr
equal = Eq empty

-- | Creates a new unification expression
eUn :: Expr -> Expr -> Expr
eUn = Un empty

-- | Creates a new negation expression
eNeg :: Expr -> Expr
eNeg = Neg empty

-- | Creates a new boolean not expression
eNot :: Expr -> Expr
eNot = Not empty

-- | Creates a new square root expression
eSqrt :: Expr -> Expr
eSqrt = Sqrt e
  where
    e = empty & annType .~ PPFloat

-- | Creates a new power expression
ePow :: Expr -> Expr -> Expr
ePow = Pow empty

-- | Creates a new multiplication expression
eMul :: Expr -> Expr -> Expr
eMul = Mul empty

-- | Creates a new float division expression
eFDiv :: Expr -> Expr -> Expr
eFDiv = FDiv empty

-- | Creates a new integer division expression
eDiv :: Expr -> Expr -> Expr
eDiv = Div empty

-- | Creates a new remainder expression
eMod :: Expr -> Expr -> Expr
eMod = Mod empty

-- | Creates a new addition expression
eAdd :: Expr -> Expr -> Expr
eAdd = Add empty

-- | Creates a new subtraction expression
eSub :: Expr -> Expr -> Expr
eSub = Sub empty

-- | Creates a new reciprocal expression
eInv :: Expr -> Expr
eInv = Inv empty

-- | Creates a new boolean and expression
eAnd :: Expr -> Expr -> Expr
eAnd = And empty

-- | Creates a new boolean or expression
eOr :: Expr -> Expr -> Expr
eOr = Or empty

-- | Creates a new list expression
eList :: [Expr] -> Expr
eList = List empty

-- | Creates a new aggr expression
eAggr :: String -> Expr -> Expr -> Expr -> Expr
eAggr = Aggr empty

-- | Creates a new 'is' expression
eIs :: Expr -> Expr -> Expr
eIs = Is empty

eNeq :: Expr -> Expr -> Expr
eNeq = Neq empty

eTrue :: Expr
eTrue = constBool True

eFalse :: Expr
eFalse = constBool False

-- | Is the expression constant (i.e. does not contain any variables)?
isConstExpr :: Expr -> Bool
isConstExpr e = L.null [x | x@(Var _ _) <- U.universe e]

varNames :: Expr -> [String]
varNames e = nub [v | (Var _ v) <- U.universe e]

-- | Turns all `And`s to a list, 
-- starting from the root of the expression
andsToList :: Expr -> [Expr]
andsToList (And _ l r) = andsToList l <> andsToList r
andsToList x = [x]

-- | Gets predicate arguments as expressions
predicateVars :: Expr -> [Expr]
predicateVars (Pred _ _ vs) = vs
predicateVars _ = error "Expecting a predicate"

-- | Gets the names of all variables in the predicate arguments
predicateVarNames :: Expr -> [String]
predicateVarNames (Pred _ _ vs) = [n | (Var _ n) <- vs]
predicateVarNames _ = error "Expecting a predicate"

-- | Gets the names of all bounded variables in the predicate arguments
predicateBoundedVarNames :: Expr -> [String]
predicateBoundedVarNames (Pred _ _ vs) = [n | (Var ann n) <- vs, ann ^. annBound]
predicateBoundedVarNames _ = error "Expecting a predicate"

-- | Gets the names of all free variables in the predicate arguments
predicateFreeVarNames :: Expr -> [String]
predicateFreeVarNames (Pred _ _ vs) = [n | (Var ann n) <- vs, not (ann ^. annBound)]
predicateFreeVarNames _ = error "Expecting a predicate"

-- | Gets the arity of a predicate (the number of arguments)
predicateArity :: Expr -> Int
predicateArity (Pred _ _ xs) = length xs
predicateArity _ = error "Expecting a predicate"

-- | Takes a set of bound variable names and then finds all the variables in
-- in the expression tree whose name is in the set and marks them bound.
annotateWithBindings :: Set String -> Expr -> Expr
annotateWithBindings s = L.foldl1 eAnd
                       . annotateWithBindings' s 
                       . andsToList

-- | Helper function for `annotateWithBindings`
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

-- | Gets the identifier, or name of the expression if possible. Only works
-- for leaves of the expression tree
identifier :: Expr -> Maybe String
identifier (Var _ n)       = Just n
identifier (ConstStr _ n)  = Just n
identifier (Pred _ n _)    = Just n
identifier (Attribute _ n) = Just n
identifier _ = Nothing

-- | Creates a new attribute 
-- It is essentially a constant, whose value comes from an external database
attribute :: String -> Expr
attribute = Attribute empty

hole :: Expr
hole = Hole empty

-- | Unifies the typings of the two expressions
unifyExprAnns :: Expr -> Expr -> Maybe Expr
unifyExprAnns x y = x & annotation %%~ (unifyAnns a)
  where a = y ^. annotation

unifyExprAnnsWithError :: Expr -> Expr -> Expr
unifyExprAnnsWithError x y = fromMaybe err $ unifyExprAnns x y
  where err = undefined

-- | Returns the result of unifying the types of the two expressions
unifyExprTypes :: Expr -> Expr -> Maybe PPType
unifyExprTypes x y = unifyTypes xd yd
  where
    xd = head $ x ^.. annotation . annType
    yd = head $ y ^.. annotation . annType

unifyExprTypesWithError :: Expr -> Expr -> PPType
unifyExprTypesWithError x y = fromMaybe err $ unifyExprTypes x y
  where err = undefined

-- | Returns the result of unifying the domains of the two expressions
unifyExprDomains :: Expr -> Expr -> PPDomain
unifyExprDomains x y = unifyDomains xd yd
  where
    xd = head $ x ^.. annotation . domain
    yd = head $ y ^.. annotation . domain

-- Unifies the given typing with the current typing of the expression
-- and then sets the result of unification as the new typing of that expression.
applyTyping :: Typing -> Expr -> Maybe Expr
applyTyping t e = e & annotation . typing %%~ unifyTypings t

-- | Returns True if expression is an arithmetic expression.
isArithmetic :: Expr -> Bool
isArithmetic (Add{}) = True
isArithmetic (Sub{}) = True
isArithmetic (Mul{}) = True
isArithmetic (FDiv{}) = True
isArithmetic (Div{}) = True
isArithmetic (Mod{}) = True
isArithmetic (Neg{}) = True
isArithmetic (Inv{}) = True
isArithmetic (Sqrt{}) = True
isArithmetic (Pow{}) = True
isArithmetic _       = False

-- | Returns True if the expression is a predicate, built-in or otherwise
isPredicative :: Expr -> Bool
isPredicative (Pred{}) = True
isPredicative (Aggr{}) = True
isPredicative (Gt{})   = True
isPredicative (Ge{})   = True
isPredicative (Eq{})   = True
isPredicative (Le{})   = True
isPredicative (Lt{})   = True
isPredicative (Is{})   = True
isPredicative (Un{})   = True
isPredicative _        = False

applyAnnotation :: Ann -> Expr -> Maybe Expr
applyAnnotation ann expr = 
  do
    ann1 <- ann & annType  %%~ unifyTypes (expr ^. annotation . annType)
    let ann2 = ann1 & domain   %~ unifyDomains (expr ^. annotation . domain)
                    & annBound .~ (expr ^. annotation . annBound)
    return $ expr & annotation .~ ann2

