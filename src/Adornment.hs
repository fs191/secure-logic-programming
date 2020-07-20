{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Adornment 
  ( Adornable(..)
  , suffixPredicate
  , adornProgram
  -- Testing
  , adornRule
  , runAdornM
  ) where

import Annotation
import DatalogProgram
import Expr
import Rule
import DBClause

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Control.Exception

import Data.Generics.Uniplate.Data as U
import Data.Set as S
import Data.List as L
import Data.Maybe

data Adornable = Adornable 
  { _aRule  :: Rule 
  , _aBound :: [Bool]
  }
  deriving (Eq, Ord, Show)

data AdornState = AdornState
  { _gsVisited   :: [Adornable]
  , _gsQueue     :: [Adornable]
  , _gsBound     :: Set String
  , _gsRules     :: [Rule]
  , _gsDBClauses :: [DBClause]
  }
type AdornM = 
  State AdornState 

makeLenses ''Adornable
makeLenses ''AdornState

goalStr :: String
goalStr = "$goal"

runAdornM :: AdornM a -> a
runAdornM x = evalState x (AdornState [] [] S.empty [] [])

-- | Suffixes rules with parameter bindings and optimizes each rule to
-- fail as early as possible
adornProgram :: DatalogProgram -> DatalogProgram
adornProgram p = runAdornM $
  do
    let _gRule = goalToRule $ p ^. dpGoal
    -- Initialize the state
    gsRules .= p ^. dpRules
    gsRules %= L.insert _gRule
    gsQueue .= [allBound _gRule]
    gsDBClauses .= p ^.. dpDBClauses 

    -- Begin iterating through the rules starting from the goal
    _adornables <- graphLoop
    let _rules = [r & ruleHead %~ suffixPredicate bp 
                    & ruleHead . paramBindings .~ bp 
                    | (Adornable r bp) <- _adornables]

    let isGoal :: Rule -> Bool
        isGoal x = fromMaybe False $ x ^? ruleHead . _Pred . _2 . to(isPrefixOf goalStr)
        _goal = head $ L.filter isGoal _rules
    return $ p & dpRules .~ L.filter (not . isGoal) _rules
               & dpGoal  .~ (_goal ^. ruleTail)

graphLoop :: AdornM [Adornable]
graphLoop = 
  do
    _visited <- use gsVisited
    _queue <- use gsQueue

    -- Loop while queue is not empty
    _empty <- isQueueEmpty
    if _empty
      then return _visited
      else do
        _next <- popQueue
        adornRule _next
        graphLoop

popQueue :: AdornM Adornable
popQueue =
  do
    _queue <- use gsQueue
    when (L.null _queue) $ error "trying to pop an empty queue"
    modify $ gsQueue %~ tail
    return $ head _queue

adornRule :: Adornable -> AdornM ()
adornRule a@(Adornable r _bp) = 
  do
    -- Add all bound variables in the rule head to gsBound
    let _args = args r `zip` _bp
    let _boundArgs = fst <$> L.filter snd _args
    let _varNameFun (Var _ n) = Just n
        _varNameFun _         = Nothing
    let _boundNames = catMaybes $ _varNameFun <$> _boundArgs
    modify $ gsBound .~ fromList _boundNames
    _bound <- use gsBound

    -- Reorder the terms in rule body
    let _terms = (andsToList $ r ^. ruleTail)
    _reordered <- reorderTerms _terms

    -- Suffix any predicates in rule body that are not database facts
    let _suffixNotEDB x =
          do
            _isEDB <- isPredEDB x
            if _isEDB
              then return x
              else return $ suffixExpr x
    _suffixed <- traverse _suffixNotEDB _reordered
    let _folded = foldr1 eAnd _suffixed

    -- Add current rule to gsVisited
    _rules <- use gsRules
    _visited <- use gsVisited
    _queue   <- use gsQueue
    let _visQueue = L.insert a $ _visited `L.union` _queue
    let _modified = a & aRule . ruleTail .~ _folded
    gsVisited %= L.insert _modified

    -- Generate new adornables and add them to the queue if not visited yet
    let _newPairs :: [Adornable]
        _newPairs = do
          let toPair p@(Pred _ n _) = Just (p, n)
              toPair _ = Nothing
          (p, n) <- nub $ _reordered ^.. folded . to toPair . _Just
          let _rs  = findRulesByName _rules n
              _bindings = repeat $ p ^. paramBindings
              _ads  = uncurry Adornable <$> zip _rs _bindings
          L.filter (not . isVisited _visQueue) _ads
    _notEDB <- filterM (fmap not . isEDB . _aRule) _newPairs
    gsQueue <>= _newPairs

    return ()

findRulesByName :: [Rule] -> String -> [Rule]
findRulesByName rs n = L.filter (\x -> n == ruleName x) rs

isQueueEmpty :: AdornM Bool
isQueueEmpty = use $ gsQueue . to L.null

isVisited :: [Adornable] -> Adornable -> Bool
isVisited visited (Adornable r bp) = anyOf folded f visited
  where
    f (Adornable r' bp') = (r ^. ruleHead == r' ^. ruleHead) && bp == bp'

-- | Suffixes a predicate based on a given binding pattern
suffixPredicate :: [Bool] -> Expr -> Expr
suffixPredicate suf = _Pred . _2 %~ (<> "_" <> showBindings suf)

predPattern :: Set String -> Expr -> [Bool]
predPattern bound (Pred _ _ as) = f <$> as
  where
    f :: Expr -> Bool
    f (Var _ n) | n `elem` bound = True
                | otherwise      = False
    f _ = True
predPattern _ _ = []

-- | Reorders the list of terms to maximize the number of
-- bound variables at each predicate.
-- The bindings annotations will be set for all predicates.
reorderTerms :: [Expr] -> AdornM [Expr]
reorderTerms [] = return []
reorderTerms l  =
  do
    _bound <- use gsBound
    -- Might be inefficient to sort again every time
    -- for longer rule bodies
    let _best = sortBy (comp _bound) l
    let _headBest = head _best
    -- Bind all the variables that are parameters of the best candidate
        _vars = [v | v@(Var _ _) <- U.universe _headBest]
        _newBound = [n | (Var _ n) <- _vars]
    let _pat = predPattern _bound _headBest
        _ad = U.transform (annotateWithBindings _bound) $ _headBest & paramBindings .~ _pat
    modify $ gsBound %~ S.union (S.fromList _newBound)
    _t <- reorderTerms $ tail $ _best
    return $ _ad:_t

comp :: S.Set String -> Expr -> Expr -> Ordering
comp bound l r = 
  compare (f ys) $ f xs
    where f x = x ^.. folded . _Var . _2 . filtered(`elem` bound)
          xs  = [v | v@(Var _ _)<- U.universeBi l]
          ys  = [v | v@(Var _ _)<- U.universeBi r]

allBound :: Rule -> Adornable
allBound r = Adornable r $ True <$ args r

-- | Suffixes the predicate based on its annotations
suffixExpr :: Expr -> Expr
suffixExpr = U.transform f
  where
    f p@(Pred _ _ _) = suffixPredicate (p ^. paramBindings) p
    f x = x

goalToRule :: Expr -> Rule
goalToRule = rule goalStr []

isPredEDB :: Expr -> AdornM Bool
isPredEDB x = use $ gsDBClauses . to (any f)
  where n = x ^? _Pred . _2
        f c = maybe False (name c ==) n

-- | Returns true if rule is an extensional database fact
isEDB :: Rule -> AdornM Bool
isEDB = view $ ruleHead . to isPredEDB

paramBindings :: Lens' Expr [Bool]
paramBindings = partsOf $ _Pred . _3 . traversed . annLens . annBound

showBindings :: [Bool] -> String
showBindings [] = ""
showBindings (True:bt)  = 'b' : showBindings bt
showBindings (False:bt) = 'f' : showBindings bt

