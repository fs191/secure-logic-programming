module MagicSets 
  ( magicSets
  ) where

import Control.Lens

import Annotation
import DatalogProgram
import Expr
import Rule

-- TODO handle goal
magicSets :: DatalogProgram -> DatalogProgram
magicSets dp = dp & dpRules .~ magRules <> modRules
  where
    rules    = dp ^. dpRules
    modRules = rules & traversed %~ modifiedRule
    magRules = concat $ magicRules <$> rules

modifiedRule :: Rule -> Rule
modifiedRule r = r & ruleTail %~ eAnd (magify $ r ^. ruleHead)

magicRules :: Rule -> [Rule]
magicRules r = magicRules' r . andsToList $ r ^. ruleTail

magicRules' :: Rule -> [Expr] -> [Rule]
magicRules' _ [] = []
magicRules' r (p@Pred{}:xt) = m : magicRules' r xt
  where
    m = Rule (magify p) $ eAnd (magify $ r ^. ruleHead) e
    e = foldr eAnd constTrue xt
magicRules' r (_:xt) = magicRules' r xt

magify :: Expr -> Expr
magify (Pred a n as) = Pred a ("m_" <> n) as'
  where
    as' = filter (view $ annotation . annBound) as
magify x = error $ "Expected a predicate, got " <> show x

