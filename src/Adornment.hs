module Adornment 
  ( Binding(..)
  , BindingPattern
  , adornProgram
  , predicatePattern
  ) where

import DatalogProgram
import Expr
import Rule

import Control.Applicative
import Control.Lens
import Control.Monad.State

import Data.Generics.Uniplate.Data as U
import Data.List
import Data.Maybe
import qualified Data.Set as S

data Binding
  = Free
  | Bound
  deriving (Show,Enum,Eq,Ord)

newtype BindingPattern = BindingPattern [Binding]

adornProgram :: DatalogProgram -> DatalogProgram
adornProgram = undefined adornRule

-- | Reorders the predicates in the rule body so that 
-- the rule fails as early as possible for the given binding pattern
adornRule :: Rule -> BindingPattern -> Rule
adornRule r pat@(BindingPattern ps) = flip evalState [] $ do
  -- Predicate name with binding pattern suffix
  let n      = r ^. ruleHead . _Pred . _2 . to (++ bindingPatternSuffix pat)
      -- Get all leaves of the `And` tree
      _terms = concat [[x, y] | (And _ x y) <- U.universe $ r ^. ruleTail]
      terms  = filter (not . isAnd) _terms
      -- Get all initially bound variables
      _bound = ps `zip` (r ^. ruleHead . _Pred . _3)
      bound  = [x | (Bound, Var _ x) <- _bound]
  let terms' = adornExprs terms $ S.fromList bound
  return $ r 
    & ruleHead . _Pred . _2 .~ n
    & ruleTail .~ foldr1 eAnd terms'

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

-- | If given a predicate, returns a binding pattern that has 'f'
-- at the position of each variable and 'b' in place of anything else
predicatePattern :: Expr -> Maybe BindingPattern
predicatePattern (Pred _ _ as) = Just . BindingPattern $ f <$> as
  where
    f (Var _ _) = Free
    f _         = Bound
predicatePattern _ = Nothing

bindingChar :: Binding -> Char
bindingChar Free  = 'f'
bindingChar Bound = 'b'

bindingPatternSuffix :: BindingPattern -> String
bindingPatternSuffix (BindingPattern l) = bindingChar <$> l

--
-- Utilities
--

isAnd :: Expr -> Bool
isAnd (And _ _ _) = True
isAnd _ = False

predicateVarNames :: Expr -> [String]
predicateVarNames (Pred _ _ vs) = [n | (Var _ n) <- vs]
predicateVarNames _ = error "Expecting a predicate"

comp :: S.Set String -> Expr -> Expr -> Ordering
comp bound (Pred _ _ xs) (Pred _ _ ys) = 
  compare (f xs) $ f ys
    where f x = x ^.. folded . _Var . _2 . filtered(flip elem bound)

