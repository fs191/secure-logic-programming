module Rule where

---------------------------------------------------------
---- Data structures for LP facts and rules
---------------------------------------------------------

import Data.List
import qualified Data.Map as M

import Aexpr

-- names of attributes, free variables, and predicates are strings
type AName = String
type VName = String
type PName = String

-- possible (classes of) types of database attributes
data VarType = VarNum | VarText deriving (Ord,Eq,Show)

-- predicate argument, together with the privacy/data type
-- here var is a database variable (not a free LP variable)
data Var = Public VarType AName | Private VarType AName | Free VName deriving (Ord,Eq,Show)
type Arg = AExpr Var

-- rule
data Rule = Rule [Arg] [RHS] deriving Show

-- a RHS predicate can be either arithetic blackbox operation (like "=") or a fact defined in LP
data RHS = Fact PName [Arg] | ABB Arg deriving Show

-- a predicate map is a mapping from a list of arguments of a predicate
-- to a boolean expresion desrribing the condtions on with those arguments satisfy the predicate
-- e.g. 'p(0). p(x) :- x < 1000.' would be expressed as a map {[0] -> True, [x] -> {x < 1000}}
type PMap = M.Map [Arg] Arg

predToString :: String -> PName -> [Arg] -> Arg -> String
predToString prefix pname args bexpr =
    prefix ++ pname ++ "(" ++ intercalate "," (map aexprToString args) ++ ") :-\n" ++ prefix ++ aexprToString bexpr ++ "."
