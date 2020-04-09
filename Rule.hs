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
data DataType   = VarBool | VarNum | VarText | Unknown deriving (Ord,Eq,Show)
data DomainType = Public  | Private                    deriving (Ord,Eq,Show)

-- predicate argument, together with the privacy/data type
-- here var is a database variable (not a free LP variable)
data Var  = Bound DomainType DataType AName | Free VName deriving (Ord,Eq,Show)
type Term = AExpr Var

-- a rule has a list of arguments and a list of formulae that comprise rule premise
-- TODO a single premise would be sufficent, as it can be an AND of other formulae
data Rule = Rule [Term] [Formula] deriving Show

-- a formula can be either boolean arithetic blackbox operation (like "=") or a fact defined in LP
-- TODO the current approach allows formulae to be non-boolean expressions, we should fix it
data Formula = Fact PName [Term] | ABB Term deriving Show

-- a predicate map is a mapping from a list of arguments of a predicate
-- to a boolean expresion desrribing the condtions on with those arguments satisfy the predicate
-- e.g. 'p(0). p(x) :- x < 1000.' would be expressed as a map {[0] -> True, [x] -> {x < 1000}}
type PMap = M.Map [Term] Term

predToString :: String -> PName -> [Term] -> Term -> String
predToString prefix pname args bexpr =
    ruleHeadToString prefix pname args ++ " :-\n" ++ prefix ++ ruleIndent ++ formulaToString prefix bexpr ++ "."

------------------------------------------------------------------------------------
ruleDomainToString Public = ""
ruleDomainToString Private = "private"

ruleTypeToString VarBool = "bool"
ruleTypeToString VarNum  = "int"
ruleTypeToString VarText = "string"
ruleTypeToString Unknown = "unknown"

ruleIndent = "  "

ruleHeadToString prefix pname args  = prefix ++ pname ++ "(" ++ intercalate "," (map (formulaToString prefix) args) ++ ")"

-- this is currently used only for visual feedback, so the syntax of printed messages is not very important
formulaToString :: String -> AExpr Var -> String
formulaToString prefix aexpr =
    case aexpr of
        AVar (Bound domainType dataType vName) -> ruleDomainToString domainType ++ " " ++ ruleTypeToString dataType ++ " " ++ vName
        AVar (Free vName) -> vName
        AConstBool b -> case b of {True -> "true"; False -> "false"}
        AConstNum c -> show c
        AConstStr s -> s

        ANary (AMember pred) args -> pred ++ "(" ++ intercalate "," (map processRec args) ++ ")"

        ANary ASum xs -> "(" ++ intercalate " + " (map processRec xs) ++ ")"
        ANary AProd xs -> "(" ++ intercalate " * " (map processRec xs) ++ ")"
        ANary AAnds xs -> ruleIndent ++ intercalate (",\n" ++ prefix ++ ruleIndent) (map processRec xs)
        ANary AOrs  xs -> ruleIndent ++ "(" ++ intercalate (";\n\n" ++ prefix ++ ruleIndent) (map processRec xs) ++ ")"

        AUnary ANeg x -> "( - " ++ processRec x ++ ")"
        AUnary ANot x -> "\\+(" ++ processRec x ++ ")"

        ABinary ADiv x1 x2 -> "(" ++ processRec x1 ++ " / " ++ processRec x2 ++ ")"
        ABinary AMult x1 x2 -> "(" ++ processRec x1 ++ " * " ++ processRec x2 ++ ")"
        ABinary AAdd x1 x2 -> "(" ++ processRec x1 ++ " + " ++ processRec x2 ++ ")"
        ABinary ASub x1 x2 -> "(" ++ processRec x1 ++ " - " ++ processRec x2 ++ ")"
        ABinary AAnd x1 x2 -> processRec x1 ++ ",\n" ++ prefix ++ ruleIndent ++ processRec x2
        ABinary AOr  x1 x2 -> processRec x1 ++ ";\n\n" ++ prefix ++ ruleIndent ++ processRec x2 ++ ")"
        ABinary ALT x1 x2  -> "(" ++ processRec x1 ++ " < " ++ processRec x2 ++ ")"
        ABinary ALE x1 x2  -> "(" ++ processRec x1 ++ " <= " ++ processRec x2 ++ ")"
        ABinary AEQ x1 x2  -> "(" ++ processRec x1 ++ " = " ++ processRec x2 ++ ")"
        ABinary AGE x1 x2  -> "(" ++ processRec x1 ++ " >= " ++ processRec x2 ++ ")"
        ABinary AGT x1 x2  -> "(" ++ processRec x1 ++ " > " ++ processRec x2 ++ ")"
        ABinary AAsgn x1 x2  -> "(" ++ processRec x1 ++ " := " ++ processRec x2 ++ ")"
     where processRec x = formulaToString prefix x


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
