module Expr.Classes where

import Relude

import Control.Lens

import Expr

class HasVarIdx a where
  idxLens :: Lens' a Int

instance HasVarIdx Int where
  idxLens = id

freshVar :: (HasVarIdx s, MonadState s m) => Text -> m Expr
freshVar n =
  do
    i <- use idxLens
    modifying idxLens (+1)
    return . var $ n <> "_" <> show i

