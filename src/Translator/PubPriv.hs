module Translator.PubPriv (pubPrivTransform) where

import Relude

import Control.Lens
import qualified Data.Generics.Uniplate.Data as U

import Annotation
import DatalogProgram
import Expr
import Rule

pubPrivTransform :: DatalogProgram -> DatalogProgram
pubPrivTransform = dpRules . traversed . ruleTail %~ U.rewrite reorderAnds

reorderAnds :: Expr -> Maybe Expr
reorderAnds (And a x y)
  | bothPublic y && (not $ bothPublic x) = Just $ And a y x
  | otherwise = Nothing
reorderAnds _ = Nothing

bothPublic :: Expr -> Bool
bothPublic e = Just True == e ^? leftHand . isPub
            && Just True == e ^? rightHand . isPub
  where isPub = annotation . domain . to (==Public)

