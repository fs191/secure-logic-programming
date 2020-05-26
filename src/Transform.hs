module Transform
  ( deriveAllGroundRules
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----  to intermediate representation
---------------------------------------------------------

import Data.Generics.Uniplate.Operations as U
import Data.Maybe

import Control.Lens

import Rule
import Expr
import Substitution
import qualified DatalogProgram as DP

-- | generate all possible ground rules for n iterations
deriveAllGroundRules :: DP.DatalogProgram -> Int -> DP.DatalogProgram
deriveAllGroundRules program n = program & DP.ruleLens %~ f
  where
    f :: [Rule] -> [Rule]
    f x = foldl (.) id (replicate n (simplify . inlineOnce)) x

inlineOnce :: [Rule] -> [Rule]
inlineOnce rs =
  rs <> do
    tgt <- refreshRule "X_" <$> rs
    src <- refreshRule "Y_" <$> rs
    let shd = src ^. ruleHead
    let stl = src ^. ruleTail
    let ttl = tgt ^. ruleTail
    (p@(Pred _ _), mut) <- U.contexts ttl
    let subst = unify shd p
    s <- maybeToList subst
    return . applySubst s $ tgt & ruleTail .~ mut stl

simplify :: [Rule] -> [Rule]
simplify = id

