{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Adornment 
  ( BindingPattern(..)
  , Binding(..)
  , Adornable(..)
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

data AdornmentException
  = NoGoal
  deriving (Show, Exception)

data Adornable = Adornable 
  { _aRule :: Rule 
  , _aPat  :: BindingPattern
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
  StateT AdornState (Except AdornmentException)

makeLenses ''Adornable
makeLenses ''AdornState

goalStr :: [Char]
goalStr = "$goal"

runAdornM :: AdornM a -> Either AdornmentException a
runAdornM x = runExcept $ evalStateT x (AdornState [] [] S.empty [] [])

-- | Suffixes rules with parameter bindings and optimizes each rule to
-- fail as early as possible
adornProgram :: DatalogProgram -> Either AdornmentException DatalogProgram
adornProgram p = runAdornM $
  do
    let _gRule = goalToRule $ p ^. dpGoal
    gsRules .= p ^. dpRules
    gsRules %= L.insert _gRule
    gsQueue .= [allBound _gRule]
    gsDBClauses .= p ^.. dpDBClauses 

    _adornables <- graphLoop
    let _rules = [r & ruleHead %~ suffixPredicate bp 
                    & ruleHead . annLens . bindings .~ Just bp 
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
adornRule a@(Adornable r bp) = 
  do
    -- Add all bound variables in the rule head to gsBound
    let (BindingPattern _bp) = bp
    let _args = args r `zip` _bp
    let _boundArgs = fst <$> L.filter ((==Bound) . snd) _args
    let _varNameFun (Var _ n) = Just n
        _varNameFun _         = Nothing
    let _boundNames = catMaybes $ _varNameFun <$> _boundArgs
    modify $ gsBound .~ fromList _boundNames
    _bound <- use gsBound

    -- Reorder the terms in rule body
    let _terms = andsToList $ r ^. ruleTail
    _reordered <- reorderTerms _terms
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
          let toPair (ann, n, _) = (ann, n)
          (ann, n) <- nub $ _reordered ^.. folded . _Pred . to(toPair)
          let _rs  = findRulesByName _rules n
              _bindings = repeat . fromJust $ ann ^. bindings
              _ads  = uncurry Adornable <$> zip _rs _bindings
          L.filter (not . isVisited _visQueue) _ads
    _notEDB <- filterM (liftM not . isEDB . _aRule) _newPairs
    gsQueue %= (<> _newPairs)

    return ()

findRulesByName :: [Rule] -> String -> [Rule]
findRulesByName rs n = L.filter (\x -> n == ruleName x) rs

isQueueEmpty :: AdornM Bool
isQueueEmpty = use $ gsQueue . to(L.null)

isVisited :: [Adornable] -> Adornable -> Bool
isVisited visited (Adornable r bp) = anyOf folded f visited
  where
    f (Adornable r' bp') = (r ^. ruleHead == r' ^. ruleHead) && bp == bp'

-- | Suffixes a predicate based on a given binding pattern
suffixPredicate :: BindingPattern -> Expr -> Expr
suffixPredicate suf = _Pred . _2 %~ (<> "_" <> show suf)

predPattern :: Set String -> Expr -> BindingPattern
predPattern bound (Pred _ _ as) = BindingPattern $ f <$> as
  where
    f :: Expr -> Binding
    f (Var _ n) | elem n bound = Bound
                | otherwise    = Free
    f _ = Bound
predPattern _ _ = BindingPattern []

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
    let _best        = sortBy (comp _bound) l
        _headBest = head _best
    -- Bind all the variables that are parameters of the best candidate
    let _vars = case _headBest of
                  (Pred _ _ x) -> x
                  x            -> [v | v@(Var _ _) <- U.universe x]

        _newBound = [n | (Var _ n) <- _vars]
    modify $ gsBound %~ S.union (S.fromList _newBound)
    _t <- reorderTerms $ tail _best
    let _pat = predPattern _bound _headBest
        _ad = _headBest & annLens . bindings .~ Just _pat
    return $ _ad:_t

comp :: S.Set String -> Expr -> Expr -> Ordering
comp bound l r = 
  compare (f ys) $ f xs
    where f x = x ^.. folded . _Var . _2 . filtered(flip elem bound)
          xs  = [v | v@(Var _ _)<- U.universeBi l]
          ys  = [v | v@(Var _ _)<- U.universeBi r]

allBound :: Rule -> Adornable
allBound r = Adornable r . BindingPattern $ const Bound <$> args r

-- | Suffixes the predicate based on its annotations
suffixExpr :: Expr -> Expr
suffixExpr = U.transform f
  where
    f p@(Pred ann _ _) = 
      case ann ^. bindings of
        Just bp -> suffixPredicate bp p
        Nothing -> p
    f x = x

goalToRule :: Expr -> Rule
goalToRule g = rule goalStr [] g

isPredEDB :: Expr -> AdornM Bool
isPredEDB x = use $ gsDBClauses . to (any (\c -> name c == n))
  where n = x ^. _Pred . _2

-- | Returns true if rule is an extensional database fact
isEDB :: Rule -> AdornM Bool
isEDB = view $ ruleHead . to isPredEDB


