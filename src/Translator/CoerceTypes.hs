module Translator.CoerceTypes (coerceTypes) where

import Relude

import Control.Lens

import Expr
import Language.Privalog.Types

coerceTypes :: Expr -> Maybe Expr
coerceTypes e =
  do
    lhs <- e ^? leftHand
    rhs <- e ^? rightHand
    let tl = lhs ^. annotation . annType
    let tr = rhs ^. annotation . annType
    let t = fromMaybe undefined $ unifyTypes tl tr
    case (tl /= t, tr /= t) of
      (True, True) -> Just $ e & leftHand  .~ eCast lhs t
                               & rightHand .~ eCast rhs t
      (True, False) -> Just $ e & leftHand  .~ eCast lhs t
      (False, True) -> Just $ e & rightHand .~ eCast rhs t
      (False, False) -> Nothing

