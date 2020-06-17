module RGGraph 
  ( RGGraph
  , BindingPattern(..)
  , Binding(..)
  , predicatePattern
  , suffixPredicate
  , programGraph
  ) where

import Annotation
import DatalogProgram
import Expr
import Rule

import Algebra.Graph

import Control.Lens

import Data.Generics.Uniplate.Data as U
import Data.Text.Prettyprint.Doc

type RGGraph = Graph RGVertex

data RGVertex
  = R Rule Int BindingPattern
  | G String BindingPattern
  deriving (Eq, Ord)

instance Show RGVertex where
  show (R r n pat) = "r" <> show n <> "_" <> show pat <> "_" <> (show . pretty $ r ^. ruleHead)
  show (G e pat) = "g_" <> show pat <> "_" <> (show . pretty $ e)

instance Pretty RGVertex where
  pretty = pretty . show

programGraph :: DatalogProgram -> RGGraph
programGraph p = overlays $ _gToR <> _rToG
  where
    _gToR :: [Graph RGVertex]
    _gToR = do
      -- Index all the rules
      (r,idx) <- rules p `zip` [0..]
      -- Find all the predicates in the rule body
      pr@(Pred _ n _) <- U.universe $ r ^. ruleTail
      let pPat = predicatePattern pr
          ruleVert = vertex $ R r idx pPat
          goalVert = vertex $ G n pPat
      return $ goalVert `connect` ruleVert 
    _rToG = do
      (r, idx) <- rules p `zip` [0..]
      let rh = r ^. ruleHead
          pPat = predicatePattern rh
          ruleVert = vertex $ R r idx pPat
          goalVert = vertex $ G (rh ^. _Pred . _2) pPat
      return $ ruleVert `connect` goalVert

-- | If given a predicate, returns a binding pattern that has 'f'
-- at the position of each variable and 'b' in place of anything else
-- TODO: Do this properly
predicatePattern :: Expr -> BindingPattern
predicatePattern (Pred _ _ as) = BindingPattern $ f <$> as
  where
    f _         = Free
predicatePattern _ = error "Expecting a predicate"

suffixPredicate :: BindingPattern -> Expr -> Expr
suffixPredicate suf = _Pred . _2 %~ (<> "_" <> show suf)

