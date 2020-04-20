module Rule
  ( Term, Formula
  , AName, VName, PName
  , Var(..)  --TODO Refactor so that we don't have to export constructors
  , Rule(..) -- Same here
  , DataType(..)   -- < This should be OK, since it's a simple datatype
  , DomainType(..)
  , PMap
  , predToString
  , ruleHeadToString
  , termToString
  ) where

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
data DataType   = VarBool | VarNum | VarText | Unknown
  deriving (Ord,Eq,Show)
data DomainType = Public  | Private
  deriving (Ord,Eq,Show)

-- predicate argument, together with the privacy/data type
-- here var is a database variable (not a free LP variable)
data Var  = Bound DomainType DataType AName | Free VName
  deriving (Ord,Eq)

instance Show Var where
  show (Free n) = n
  show (Bound t d n) = n ++ "<bound: " ++ (show t) ++ ", " ++ (show d) ++ ">"

type Term    = AExpr Var
type Formula = BExpr Var

-- a rule has a list of arguments and a formula that represents rule premise
data Rule = Rule [Term] Formula

instance Show Rule where
  show (Rule terms formula) = "\t(" ++ (intercalate ", " $ show <$> terms) ++ ") :- " ++ (show formula) ++ "\n"

-- a predicate map is a mapping from a list of arguments of a predicate
-- to a boolean expresion desrribing the condtions on with those arguments satisfy the predicate
-- e.g. 'p(0). p(x) :- x < 1000.' would be expressed as a map {[0] -> True, [x] -> {x < 1000}}
type PMap = M.Map [Term] Formula

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

--------------------------------
-- this is for debugging only
showFactMap :: (M.Map PName PMap) -> String
showFactMap facts =
  let res = map (\p ->
                     "%% [[ " ++ p ++ "]] %% \n"
                     ++ intercalate "\n\n" (map (\key -> predToString "" p key ((facts M.! p) M.! key) ++ "\n") (M.keys (facts M.! p)))
                ) (M.keys facts)
  in
  intercalate "\n" res
