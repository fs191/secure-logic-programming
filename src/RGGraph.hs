module RGGraph 
  ( RGGraph
  , BindingPattern
  , Binding(..)
  , predicatePattern
  , suffixPredicate
  ) where

import DatalogProgram
import Expr
import Rule

import Algebra.Graph

import Control.Lens

import Data.Generics.Uniplate.Data as U

type BindingPattern = [Binding]

data Binding
  = Free
  | Bound
  deriving (Show,Enum,Eq,Ord)

type RGGraph = Graph RGVertex

data RGVertex
  = R Rule BindingPattern
  | G Expr BindingPattern

programGraph :: DatalogProgram -> Maybe RGGraph
programGraph p = 
  do
    g <- programGoalExpr p
    let goalPreds = [G x $ predicatePattern x | x@(Pred _ _ _) <- U.universe g]
    undefined

-- | If given a predicate, returns a binding pattern that has 'f'
-- at the position of each variable and 'b' in place of anything else
predicatePattern :: Expr -> BindingPattern
predicatePattern (Pred _ _ as) = f <$> as
  where
    f (Var _ _) = Free
    f _         = Bound
predicatePattern _ = error "Expecting a predicate"

bindingChar :: Binding -> Char
bindingChar Free  = 'f'
bindingChar Bound = 'b'

bindingPatternSuffix :: BindingPattern -> String
bindingPatternSuffix l = bindingChar <$> l

suffixPredicate :: BindingPattern -> Expr -> Expr
suffixPredicate suf = _Pred . _2 %~ (<> "_" <> bindingPatternSuffix suf)

