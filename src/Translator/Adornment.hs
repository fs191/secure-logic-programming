{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Translator.Adornment 
  ( Adornable(..)
  , suffixPredicate
  , adornProgram
  , adornRule
  , allAdornables
  , aBound
  , showBindings
  ) where

import Relude

import Annotation
import DatalogProgram
import Expr
import Rule

import Control.Lens

import Data.Generics.Uniplate.Data as U
import qualified Data.Set as S
import qualified Data.List as L hiding (nub)
import qualified Data.Text as T

data Adornable = Adornable 
  { _aRule   :: Rule 
  , _aBound  :: [Bool]
  , _aUsedAt :: Expr
  }
  deriving (Eq, Ord, Show)

data AdornState = AdornState
  { _gsVisited   :: [Adornable]
  , _gsQueue     :: [Adornable]
  , _gsBound     :: Set Text
  , _gsRules     :: [Rule]
  , _gsDBClauses :: [Expr]
  }
type AdornM = 
  State AdornState 

makeLenses ''Adornable
makeLenses ''AdornState

goalStr :: Text
goalStr = "$goal"

runAdornM :: AdornM a -> a
runAdornM x = evalState x (AdornState [] [] S.empty [] [])

initialize :: DatalogProgram -> AdornM ()
initialize p = 
  do
    let _gRule = goalToRule $ p ^. dpGoal
    -- Initialize the state
    gsRules .= p ^. dpRules
    gsRules %= L.insert _gRule
    gsQueue .= [allBound _gRule (p ^. dpGoal)]
    gsDBClauses .= p ^.. dpDBClauses 

-- | Assigns correct bindings to variables. Creates new rules based on
-- which arguments are bound when the rule gets called
adornProgram :: DatalogProgram -> DatalogProgram
adornProgram p = runAdornM $
  do
    initialize p

    -- Begin iterating through the rules starting from the goal
    _adornables <- graphLoop
    let _rules = [r & ruleHead %~ suffixPredicate bp 
                    & ruleHead . paramBindings .~ bp 
                    | (Adornable r bp _) <- _adornables]

    let isGoal :: Rule -> Bool
        isGoal x = Just True == x ^? ruleHead . _Pred . _2 . to(T.isPrefixOf goalStr)
        _goal = L.head $ L.filter isGoal _rules
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
        adornRule' _next
        graphLoop

popQueue :: AdornM Adornable
popQueue =
  do
    _queue <- use gsQueue
    when (L.null _queue) $ error "trying to pop an empty queue"
    modify $ gsQueue %~ tail . fromMaybe undefined . nonEmpty
    return $ head . fromMaybe undefined $ nonEmpty _queue

adornRule :: Rule -> Rule
adornRule r = runAdornM $
  do
    -- Add all bound variables in the rule head to gsBound
    let _bp = r ^. ruleHead . paramBindings
    let _args = args r `zip` _bp
    let _boundArgs = fst <$> L.filter snd _args
    let _varNameFun (Var _ n) = Just n
        _varNameFun _         = Nothing
    let _boundNames = catMaybes $ _varNameFun <$> _boundArgs
    modify $ gsBound .~ S.fromList _boundNames
    _bound <- use gsBound

    -- Reorder the terms in rule body
    let _terms = (andsToList $ r ^. ruleTail)
    _boundTerms <- bindTerms _terms
    return $ r & ruleTail .~ foldWithAnds _boundTerms

adornRule' :: Adornable -> AdornM ()
adornRule' a@(Adornable r _bp _) = 
  do
    -- Add all bound variables in the rule head to gsBound
    let _args = args r `zip` _bp
    let _boundArgs = fst <$> L.filter snd _args
    let _varNameFun (Var _ n) = Just n
        _varNameFun _         = Nothing
    let _boundNames = catMaybes $ _varNameFun <$> _boundArgs
    modify $ gsBound .~ S.fromList _boundNames
    _bound <- use gsBound

    -- Reorder the terms in rule body
    let _terms = (andsToList $ r ^. ruleTail)
    _boundTerms <- bindTerms _terms

    -- Suffix any predicates in rule body that are not database facts
    let _suffixNotEDB x =
          do
            _isEDB <- isPredEDB x
            if _isEDB
              then return x
              else return $ suffixExpr x
    _suffixed <- traverse _suffixNotEDB _boundTerms
    let _folded = L.foldr1 eAnd _suffixed

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
          (p, n) <- ordNub $ _boundTerms ^.. folded . to toPair . _Just
          let _rs  = findRulesByName _rules n
              _bindings = repeat $ p ^. paramBindings
              _ads  = (\(r', b) -> Adornable r' b p) <$> zip _rs _bindings
          L.filter (not . isVisited _visQueue) _ads
    _notEDB <- filterM (fmap not . isEDB . _aRule) _newPairs
    gsQueue <>= _newPairs

    return ()

findRulesByName :: [Rule] -> Text -> [Rule]
findRulesByName rs n = L.filter (\x -> n == ruleName x) rs

isQueueEmpty :: AdornM Bool
isQueueEmpty = use $ gsQueue . to L.null

isVisited :: [Adornable] -> Adornable -> Bool
isVisited visited (Adornable r bp _) = anyOf folded f visited
  where
    f (Adornable r' bp' _) = (r ^. ruleHead == r' ^. ruleHead) && bp == bp'

-- | Suffixes a predicate based on a given binding pattern. A value of `True` at
-- the index `n` means that the n-th argument of the predicate is bound.
suffixPredicate :: [Bool] -> Expr -> Expr
suffixPredicate suf = _Pred . _2 %~ (<> "_" <> showBindings suf)

predPattern :: Set Text -> Expr -> [Bool]
predPattern bound (Pred _ _ as) = f <$> as
  where
    f :: Expr -> Bool
    f (Var _ n) | n `S.member` bound = True
                | otherwise      = False
    -- Holes are treated as fresh variables, hence unbound
    f (Hole _) = False
    -- Constants are always bound
    f _ = True
predPattern _ _ = []

-- | Reorders the list of terms to maximize the number of
-- bound variables at each predicate.
-- The bindings annotations will be set for all predicates.
bindTerms :: [Expr] -> AdornM [Expr]
bindTerms [] = return []
bindTerms l  =
  do
    _bound <- use gsBound
    let _head = L.head l
    -- Bind all the variables that are parameters of the best candidate
        _vars = [v | v@(Var _ _) <- U.universe _head]
        _newBound = [n | (Var _ n) <- _vars]
    let _pat = predPattern _bound _head
        _ad = U.transform (annotateWithBindings _bound) $ _head & paramBindings .~ _pat
    modify $ gsBound %~ S.union (S.fromList _newBound)
    _t <- bindTerms $ L.tail l
    return $ _ad:_t

allBound :: Rule -> Expr -> Adornable
allBound r e = Adornable r (True <$ args r) e

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
        f c = Just (c ^. predName) == n

-- | Returns true if rule is an extensional database fact
isEDB :: Rule -> AdornM Bool
isEDB = view $ ruleHead . to isPredEDB

paramBindings :: Lens' Expr [Bool]
paramBindings = partsOf $ _Pred . _3 . traversed . annotation . annBound

showBindings :: [Bool] -> Text
showBindings = toText . map f
  where f True  = 'b'
        f False = 'f'

allAdornables :: DatalogProgram -> [Adornable]
allAdornables p = ordNub . runAdornM $
  do
    initialize p
    graphLoop

