module Translator.Distribute (distribute) where

import Relude

import qualified Data.Generics.Uniplate.Data as U
import qualified Data.List as L

import Expr

distribute :: Expr -> Expr
distribute = U.rewrite f
  where 
    f :: Expr -> Maybe Expr
    f e@Or{} 
      | res == e  = Nothing
      | otherwise = Just res
      where
        orsList = fmap andsToList $ orsToList e
        cmp x y = L.head x `exprEq` L.head y
        grouped = L.groupBy cmp orsList
        undist :: [[Expr]] -> Expr
        undist x = eAnd (L.head $ L.head x) . listToOrs $ fmap (listToAnds . L.tail) x
        res = listToOrs $ undist <$> grouped
    f _ = Nothing



