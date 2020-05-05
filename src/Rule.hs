{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rule
  ( Term, Formula
  , AName, VName, PName
  , DBVar(..) -- TODO try not to export constructor
  , Rule, Fact
  , IsRule
  , DataType(..)
  , DomainType(..)
  , predToString
  , ruleHeadToString
  , termToString
  , rule, fact
  , free, bound
  , isFree, dataType
  , rename
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
import Data.String
import Data.Maybe (isJust, catMaybes)
import qualified Data.Map as M

import Aexpr

-- names of attributes, free variables, and predicates are strings
type AName = String
type VName = String
type PName = String

-- possible (classes of) types of database attributes
data DataType   = VarBool | VarNum | VarText | Unknown
  deriving (Ord,Eq,Show)
data DomainType = Public  | Private
  deriving (Ord,Eq,Show)

-- predicate argument, together with the privacy/data type
-- here var is a database variable (not a free LP variable)
data DBVar
  = Bound DomainType DataType AName
  | Free VName
  deriving (Show, Ord, Eq)

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

predToString :: String -> PName -> [Term] -> Formula -> String
predToString prefix pname args bexpr =
    ruleHeadToString prefix pname args ++ " :-\n" ++ prefix ++ ruleIndent ++ formulaToString prefix bexpr ++ "."

------------------------------------------------------------------------------------
-- this is currently used only for visual feedback, so the syntax of printed messages is not very important
ruleDomainToString Public = ""
ruleDomainToString Private = "private"

ruleTypeToString VarBool = "bool"
ruleTypeToString VarNum  = "int"
ruleTypeToString VarText = "string"
ruleTypeToString Unknown = "unknown"

ruleHeadToString prefix pname args  = prefix ++ pname ++ "(" ++ intercalate "," (map termToString args) ++ ")"

formulaToString :: String -> Formula -> String
formulaToString prefix bexpr =
    bexprToString prefix f bexpr
    where f x = case x of
                    (Bound domainType dataType vName) -> ruleDomainToString domainType ++ " " ++ ruleTypeToString dataType ++ " " ++ vName
                    (Free vName) -> vName

termToString :: Term -> String
termToString aexpr =
    aexprToString f aexpr
    where f x = case x of
                    (Bound domainType dataType vName) -> ruleDomainToString domainType ++ " " ++ ruleTypeToString dataType ++ " " ++ vName
                    (Free vName) -> vName

rule :: Atom -> [Term] -> Formula -> Rule
rule a t f = Rule f $ fact a t

fact :: Atom -> [Term] -> Fact
fact = Fact

free :: VName -> DBVar
free = Free

bound :: DomainType -> DataType -> VName -> DBVar
bound = Bound

rename :: String -> DBVar -> DBVar
rename n (Free _) = Free n
rename n (Bound x y _) = Bound x y n

isFree :: DBVar -> Bool
isFree (Free _) = True
isFree _        = False

dataType :: DBVar -> Maybe DataType
dataType (Bound _ dt _) = Just dt
dataType _              = Nothing

toFact :: Rule -> Maybe Fact
toFact (Rule (BConstBool True) f) = Just f
toFact _                          = Nothing

isFact :: Rule -> Bool
isFact = isJust . toFact

name :: Rule -> Atom
name = _functor . _fact

rulesToFacts :: [Rule] -> [Fact]
rulesToFacts r = catMaybes $ toFact <$> r

{-# DEPRECATED toPMapMap "Avoid using this function, it will be removed in the future" #-}
toPMapMap :: [Fact] -> M.Map PName (M.Map [Term] Formula)
toPMapMap facts = M.unions $
  do
    f <- facts
    let n = functor f
        p = premise f
        a = args f
        pmap = M.singleton a p
    return $ M.singleton n pmap

{-# DEPRECATED fromPMapMap "Avoid using this function, it will be removed in the future" #-}
fromPMapMap :: M.Map PName (M.Map [Term] Formula) -> [Rule]
fromPMapMap pmap =
  do
    (x, y) <- M.toList $ M.toList <$> pmap :: [(PName, [([Term], Formula)])]
    (n, ts, f) <-
      do
        (ts', f') <- y
        return (x, ts', f')
    return $ rule n ts f
