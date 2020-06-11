module Adornment 
  ( Binding(..)
  , BindingPattern
  , adornProgram
  ) where

import DatalogProgram
import Expr
import RGGraph
import Rule

import Control.Applicative
import Control.Lens
import Control.Monad.State

import Data.Generics.Uniplate.Data as U
import Data.List
import Data.Maybe
import qualified Data.Set as S

data Adornment = Adornment String BindingPattern
  deriving (Eq)

adornProgram :: DatalogProgram -> Maybe DatalogProgram
adornProgram p = 
  do 
    e <- programGoalExpr p
    let rs = rules p
        pairs = adornments e
    return $ p & dpRules .~ adornRules rs pairs

adornRules :: [Rule] -> [Adornment] -> [Rule]
adornRules _ [] = []
adornRules rs ((Adornment n p):ts) = nub $ adorned <> adornRules rs (ts <> ts')
  where 
    adornables :: [Rule]
    adornables = filter (\x -> ruleName x == n) rs
    adorned :: [Rule]
    adorned = flip adornRule p <$> adornables
    ands :: [Expr]
    ands = concat $ adorned ^.. folded . ruleTail . to andsToList
    ts' = nub . concat $ adornments <$> ands

adornments :: Expr -> [Adornment]
adornments e = [Adornment n $ predicatePattern x | x@(Pred _ n _) <- preds]
  where
    preds = [x | x@(Pred _ _ _) <- U.universe e]

-- | Reorders the predicates in the rule body so that 
-- the rule fails as early as possible for the given binding pattern
adornRule :: Rule -> BindingPattern -> Rule
adornRule r pat = flip evalState [] $ do
      -- Get all leaves of the `And` tree
      -- Get all initially bound variables
  let terms = andsToList $ r ^. ruleTail
      _bound = pat `zip` (r ^. ruleHead . _Pred . _3)
      bound  = [x | (Bound, Var _ x) <- _bound]
      terms' = adornExprs terms $ S.fromList bound
  return $ r 
    & ruleTail .~ foldr1 eAnd terms'
    & ruleHead %~ suffixPredicate pat

adornExprs :: [Expr] -> S.Set String -> [Expr]
adornExprs exprs bound = fromJust $ evalStateT (many action) (exprs, bound)
  where 
    action :: StateT ([Expr], S.Set String) Maybe Expr
    action = 
      do
        remain <- use _1
        _bound <- use _2
        guard . not $ null remain
        let sorted = sortBy (comp _bound) remain
        modify $ _2 %~ (<> S.fromList (predicateVarNames $ head sorted))
        modify $ _1 .~ tail sorted
        return $ head sorted


--
-- Utilities
--

comp :: S.Set String -> Expr -> Expr -> Ordering
comp bound (Pred _ _ xs) (Pred _ _ ys) = 
  compare (f xs) $ f ys
    where f x = x ^.. folded . _Var . _2 . filtered(flip elem bound)

