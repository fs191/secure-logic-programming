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

import Algebra.Graph as G

import Control.Lens

import Data.Generics.Uniplate.Data as U
import Data.Text.Prettyprint.Doc
import Data.Set
import Data.Maybe

type RGGraph = Graph RGVertex

data RGVertex
  = R Rule Int BindingPattern
  | G String BindingPattern
  deriving (Eq, Ord)

instance Show RGVertex where
  show (R r n pat) 
    = "r" 
    <> show n 
    <> "_" 
    <> show pat 
    <> "_" 
    <> (show . pretty $ r ^. ruleHead)
  show (G e pat) = "g_" <> show pat <> "_" <> (show . pretty $ e)

instance Pretty RGVertex where
  pretty = pretty . show

programGraph :: DatalogProgram -> RGGraph
programGraph p = reduce . overlays $ _gToR <> _rToG
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
      -- Connect all rules to the goal with same name
      (r, idx) <- rules p `zip` [0..]
      let rh = r ^. ruleHead
          pPat = predicatePattern rh
          ruleVert = vertex $ R r idx pPat
          goalVert = vertex $ G (rh ^. _Pred . _2) pPat
      return $ ruleVert `connect` goalVert
    bodies :: [Expr]
    bodies = catMaybes [goal p] ^.. traversed . gFormula
    g :: Set RGVertex
    g = fromList $ [G n $ predicatePattern pr | pr@(Pred _ n _) <- U.universeBi bodies]
    reduce = transpose . flood g . transpose

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

flood :: (Ord a) => Set a -> Graph a -> Graph a
flood startVertex inGraph 
  | length newVertices == 0 = induce (`member` startVertex) inGraph
  | otherwise               = flood (startVertex `union` newVertices) inGraph
  where (Just ctx)  = context (`member` startVertex) inGraph
        newVertices = (fromList $ G.outputs ctx) `difference` startVertex

