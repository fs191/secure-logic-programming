{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rule
  ( Term, Formula
  , Rule, Fact
  , IsRule
  , rule, fact
  , toFact, toRule
  , name
  , premise, functor, args
  , rulesToFacts
  , toMap
  , toPMapMap, fromPMapMap
) where

---------------------------------------------------------
---- Data structures for LP facts and rules
---------------------------------------------------------

import Data.List
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Map as M

import Data.Traversable (sequenceA)

import Aexpr
import Table
import DBClause

type Term    = AExpr DBVar
type Formula = BExpr DBVar

-- a rule has a list of arguments and a formula that represents rule premise
type Atom = String

class IsRule a where
  toRule  :: a -> Rule
  premise :: a -> Formula
  functor :: a -> Atom
  args    :: a -> [Term]
  toMap   :: [a] -> M.Map String [a]

  premise  = premise . toRule
  functor  = functor . toRule
  args     = args . toRule
  toMap rs = M.unionsWith (<>) $
    do
      f <- rs
      let n = functor f
      return $ M.singleton n [f]

data Rule = Rule
  { _premise   :: Formula
  , _fact      :: Fact
  }
  deriving (Show)

data Fact = Fact
  { _functor   :: Atom
  , _arguments :: [Term]
  }
  deriving (Show)

instance IsRule Rule where
  toRule = id
  premise = _premise
  functor = _functor . _fact
  args = _arguments . _fact

instance IsRule Fact where
  toRule = Rule (BConstBool True)

fact :: Atom -> [Term] -> Fact
fact = Fact

rule :: String -> [Term] -> Formula -> Rule
rule n a f = Rule f $ fact n a

toFact :: Rule -> Maybe Fact
toFact (Rule (BConstBool True) f) = Just f
toFact _                          = Nothing

name :: Rule -> Atom
name = _functor . _fact

rulesToFacts :: [Rule] -> [Fact]
rulesToFacts r = catMaybes $ toFact <$> r

{-# DEPRECATED toPMapMap "Avoid using this function, it will be removed in the future" #-}
toPMapMap :: [Fact] -> M.Map String (M.Map [Term] Formula)
toPMapMap facts = M.unions $
  do
    f <- facts
    let n = functor f
        p = premise f
        a = args f
        pmap = M.singleton a p
    return $ M.singleton n pmap

{-# DEPRECATED fromPMapMap "Avoid using this function, it will be removed in the future" #-}
fromPMapMap :: M.Map String (M.Map [Term] Formula) -> [Rule]
fromPMapMap pmap =
  do
    (x, y) <- M.toList $ M.toList <$> pmap :: [(String, [([Term], Formula)])]
    (n, ts, f) <-
      do
        (ts', f') <- y
        return (x, ts', f')
    return $ rule n ts f

