module Transform
  ( deriveAllGroundRules
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----  to intermediate representation
---------------------------------------------------------

import qualified Data.Map as M

import Data.Generics.Uniplate as U
import Data.Maybe

import Control.Lens

import Rule
import Expr
import Substitution
import DBClause
import qualified DatalogProgram as DP

-- generate all possible ground rules for n iterations
deriveAllGroundRules :: DP.DatalogProgram -> Int -> DP.DatalogProgram
deriveAllGroundRules program n = program & DP.ruleLens %~ f n
  where
    f :: Int -> [Rule] -> [Rule]
    f n = foldl (.) id $ replicate n inlineOnce

inlineOnce :: [Rule] -> [Rule]
inlineOnce r = (ruleTail %~ f r) <$> r
  where 
    f :: [Rule] -> Expr DBVar -> Expr DBVar
    f r = foldl (.) id (inlineExpr <$> r)

inlineExpr :: Rule -> Expr DBVar -> Expr DBVar
inlineExpr r = U.transform $ \x ->
  case x of 
    p@(Pred n as) -> applyToExpr (unified p) p
    x             -> x
  where
    unified p = fromMaybe emptyTheta $ unify p (r ^. ruleHead)

