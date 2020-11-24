module Translator.PubPriv (pubPrivTransform) where

import Relude

import Control.Exception
import Control.Lens
import qualified Data.Generics.Uniplate.Data as U

import qualified Data.List as L

import Translator.Adornment

import Annotation
import DatalogProgram
import Expr
import Rule

pubPrivTransform :: DatalogProgram -> DatalogProgram
pubPrivTransform = dpRules . traversed %~ transformRuleTail

transformRuleTail :: Rule -> Rule
transformRuleTail r = assert assertion $ r & ruleTail .~ newTail
                                           & id %~ adornRule
  where tailList = r ^. ruleTail . to andsToList
        assertion = all isAsgn $ drop idx tailList
        idx = fromMaybe (length tailList) $ L.findIndex isAsgn tailList
        otherExprs = take idx tailList
        asgns = drop idx tailList
        (a, b) = L.partition bothPublic asgns
        newTail = listToAnds $ otherExprs <> a <> b

bothPublic :: Expr -> Bool
bothPublic e = Just True == e ^? leftHand . isPub
            && Just True == e ^? rightHand . isPub
  where isPub = annotation . domain . to (==Public)

privateCount' :: Expr -> Int
privateCount' e = length $
  [e' | e' <- U.universe e] ^.. folded . annotation . annBound . to (==True)

isAsgn :: Expr -> Bool
isAsgn Is{} = True
isAsgn Un{} = True
isAsgn Eq{} = True
isAsgn _    = False

