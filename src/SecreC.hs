module SecreC
  ( generateSecreCscript
  ) where

---------------------------------------------------------
---- Transformation of intermediate representation
----  to SecreC
---------------------------------------------------------

-- TODO this module is very messy and needs thorough refactoring
import Data.List
import Data.Maybe
import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr
import ErrorMsg
import Rule
import DatalogProgram
import DBClause

type PMap = M.Map [Term] Formula

indent = "    "
bexprPrefix = "b_"
aexprPrefix = "x_"
colPrefix  = "col_"
argPrefix  = "arg_"
resPrefix  = "res_"
goalPrefix = "goal_"
cntPrefix  = "n_"
piPrefix   = "pi_"

strColumn s = s ++ "_str"
tempVar s = s ++ "_tmp"
dimPar s = " [[" ++ s ++ "]] "

defaultHeader = [
    -- import essentials
    "import stdlib;",
    "import shared3p;",
    "import shared3p_string;",
    "import shared3p_table_database;",
    "import table_database;\n",
    "import lp_essentials;\n",
    "domain pd_shared3p shared3p;\n"]

defaultGoal = ["void main(){",
               -- Sharemind gives confusing error messages if the program does not use any private variables
               indent ++ "pd_shared3p uint32 dummy;",
               indent ++ "//TODO: state your own goal here",
               indent ++ "publish(\"NOTICE: no goal specified in the main function\",false);",
               "}"]

generateGoal :: Bool -> [String] -> [DomainType] -> Maybe (String,[Term]) -> [String]
generateGoal boolOnly structTypes ds goal =
    case goal of
        Just (pname, args') ->
            let is = [0..length args'-1] in
            let js = [0..length ds-1] in
            let dd = if elem Private ds then Private else Public in
            let (args,declarations) = unzip $ concat $ zipWith generateGoalArg args' is in

            let callGoals = if boolOnly then
                                let bs = map (\j -> bexprPrefix ++ show j) js in
                                let s0 = zipWith3 (\b d j -> domainToMainDecl d ++ "bool " ++ b ++ " = " ++ goalPrefix
                                                             ++ pname ++ "_" ++ show j ++ "(" ++ intercalate "," args ++ ");") bs ds js in
                                let s1 = ["publish(\"yes/no_answer\", " ++ declassify dd Public ++ "(" ++ intercalate " | " bs ++ "));"] in
                                s0 ++ s1
                            else

                                let bs = map (\j -> resPrefix ++ show j) js in
                                let fvars = map isFreeVar args' in
                                let is' = map snd $ filter (\(fv,_) -> fv) $ zip fvars is in
                                let as = map (\i -> argPrefix ++ show i) is' in
                                let s0 = zipWith4 (\b d j st -> "public " ++ st ++ " " ++ b ++ " = " ++ goalPrefix
                                                                 ++ pname ++ "_" ++ show j ++ "(" ++ intercalate "," args ++ ");") bs ds js structTypes in

                                let s1 = ["publish(\"yes/no answer\", " ++ declassify dd Public ++ "(" ++ intercalate " | " (map (\b -> "any(" ++ b ++ ".b)") bs) ++ "));"] in
                                let s2 = if length as == 0 then []
                                         else concat $ zipWith3 (\b d j -> ["uint32 " ++ cntPrefix ++ show j ++ " = " ++ declassify d Public ++ "(sum((uint32)" ++ b ++ ".b));",
                                                                       domainToMainDecl d ++ "uint32 [[1]] " ++ piPrefix ++ show j ++ "  = countSortPermutation(!" ++ b ++ ".b);"]
                                                          ) bs ds js in
                                let s3 = if length as == 0 then []
                                         else concat $ zipWith3 (\b d j -> map (\a -> "publish(\"valuation_" ++ show j ++ "_" ++ a
                                                                             ++ "\", " ++ declassify d Public ++ "(mySlice(applyPermutation(" ++ b ++ "." ++ a
                                                                             ++ "," ++ piPrefix ++ show j ++ "), 0, " ++ cntPrefix ++ show j ++ ")));") as
                                                          ) bs ds js in

                                s0 ++ s1 ++ s2 ++ s3

            in (["void main(){"] ++ (map (indent ++) (declarations ++ callGoals)) ++ ["}"])

        _ -> defaultGoal

generateGoalArg :: Term -> Int -> [(String,String)]
generateGoalArg arg i =
    let argName = argPrefix ++ show i in
    case arg of

        -- a constant is hard-coded
        AConstNum  v -> [(argName, domainToMainDecl Public ++ "bool "   ++ argName ++ " = " ++ show v ++ ";")]
        AConstStr  v -> [(argName, domainToMainDecl Public ++ "uint32 " ++ argName ++ " = " ++ declassify Private Public ++ "(CRC32(bl_str(" ++ v ++ ") :: pd_shared3p xor_uint8));")]

        -- a bounded argument comes as an input
        AVar (Bound domain  VarNum  x) -> [(argName, domainToMainDecl domain  ++ "int32 "      ++ argName ++ " = argument(" ++ show x ++ ");")]
        AVar (Bound Private VarText x) -> [(argName, domainToMainDecl Private ++ "xor_uint32 " ++ argName ++ " = CRC32(bl_str(argument(" ++ show x ++ ")) :: pd_shared3p xor_uint8);")]
        AVar (Bound Public  VarText x) -> [(argName, domainToMainDecl Public  ++ "uint32 "     ++ argName ++ " = " ++ declassify Private Public ++ "(CRC32(bl_str(argument(" ++ show x ++ ")) :: pd_shared3p xor_uint8));")]

        -- a free argument can be evaluated to anything and is not instantiated
        AVar (Free z) -> []

generateStruct :: [(Bool,Term,DomainType,DataType,Int)] -> String -> [String]
generateStruct freeArgs structName =

    let template = generateTemplateDecl True freeArgs in
    [template, "struct " ++ structName, "{", "D bool [[1]] b;"]
        ++ map generateStructArg freeArgs
        ++ ["}"]

generateStructArg :: (Bool,Term,DomainType,DataType,Int) -> String
generateStructArg (_,a,d,t,i) =
    domainToStructDecl d ++ typeToString False d t i ++ dimPar (typeToDim False d t i) ++ argPrefix ++ show i ++ ";"

generateReturn freeArgs structType =
    ["public " ++ structType ++ " result;", "result.b = b;"]
        ++ map generateReturnArg freeArgs
        ++ ["return(result);"]

generateReturnArg :: (Bool,Term,DomainType,DataType,Int) -> String
generateReturnArg (_,_,_,t,i) =
    let argName = argPrefix ++ show i in
    "result." ++ argName ++ " = " ++ argName ++ ";"

-- we need a domain template D only if private variables are used at all
generateTemplateDecl cond args =
    --let templateDomain = if cond && elem Private (map (\(_,_,d,_,_) -> d) args) then ["domain D "] else [] in
    let s0 = if cond then ["domain D "] else [] in
    let s1 = map (\(_,_,_,_,i) -> "type T" ++ show i) $ filter (\(_,_,_,t,_) -> t == Unknown) args in
    let s2 = map (\(_,_,_,_,i) -> "dim N" ++ show i) $ filter (\(_,_,_,t,_) -> t == Unknown) args in
    let template = s0 ++ s1 ++ s2 in
    if length template > 0 then "template <" ++ intercalate ", " template ++ ">" else ""

generateTemplateUse cond args =
    let s0 = if cond || elem Private (map (\(_,_,d,_,_) -> d) args) then ["pd_shared3p"] else ["public"] in
    let s1 = map (\(_,_,d,t,i) -> typeToString False d t i) args in
    let s2 = map (\(_,_,d,t,i) -> typeToDim False d t i) args in
    let template = s0 ++ s1 ++ s2 in
    if length template > 0 then "<" ++ intercalate ", " template ++ ">" else ""

-- a SecreC program is a list of code lines
-- if no particular goal is given, then we do not create a main statement
generateSecreCscript :: Bool -> PPDatalogProgram -> String
generateSecreCscript boolOnly program =
    let predMap = toPMapMap $ facts program :: M.Map String PMap

    -- TODO think whether we want to support more expressions in a goal
    -- TODO we currently treat all inputs as strings, we need to genralize it (also in plain prolog)
        goal_ :: Maybe (String, [AExpr DBVar])
        goal_ =
          do
            g <- goal program
            let xs = inputs g
                ys = outputs g
                goals = [formula g]
            guard $ length goals > 0
            case head goals of
                BListPred (BPredName gn) ga ->
                              --let xs' = intercalate "," (map termToString xs) in
                              --let ys' = intercalate "," (map termToString ys) in
                              --trace ("goal([" ++ xs' ++ "],[" ++ ys' ++ "]) :- " ++ ruleHeadToString "" gn ga) $
                              let goalArgs = map (\x -> if elem x xs then
                                                            case x of
                                                                AVar (Free z) -> AVar (Bound Private VarText z)
                                                                _             -> x
                                                        else x
                                                 ) ga in
                              Just (gn, goalArgs)
                _          -> Nothing
    in
    let header = defaultHeader in

    -- if the goal exists, generate a 'struct' for its outputs
    let (structDef,structName) = fromMaybe ([], "") $
          do
            (pname, as) <- goal_
            guard boolOnly
            let structName = resPrefix ++ pname
                fvars = map isFreeVar as
                ds = map deriveDomain as
                ts = map deriveType as
                is = [0..length as - 1]
                allArgs  = zip5 fvars as ds ts is
                freeArgs = filter (\(b,_,_,_,_) -> b) allArgs
                structDef = generateStruct freeArgs structName
            return (structDef,structName)
    in

    let (ds,structTypes,body) = unzip3 $ concat $ M.mapWithKey (createSecreCFuns boolOnly goal_ structName) predMap in
    let mainFun = generateGoal boolOnly structTypes ds goal_ in

    intercalate "\n" $ header ++ structDef ++ concat body ++ mainFun

createSecreCFuns :: Bool -> Maybe (String, [Term]) -> String -> String -> PMap -> [(DomainType, String, [String])]
createSecreCFuns boolOnly goal structName pname pmap =
    let unnecessaryFun = case goal of {Just (goalPname, _) -> pname /= goalPname; _ -> False} in
    if unnecessaryFun then [] else
    let (keys,values) = unzip (M.toList pmap) in
    let is = [0..length keys - 1] in
    let as = case goal of {Just (_, goalArgs) -> goalArgs; _ -> []} in
    zipWith3 (createSecreCFun boolOnly pname structName as) is keys values

createSecreCFun :: Bool -> String -> String -> [Term] -> Int -> [Term] -> Formula -> (DomainType, String, [String])
createSecreCFun boolOnly pname structName asG index as bexpr' =

    --if not(isGround bexpr) then [] else
    let funName = goalPrefix ++ pname ++ "_" ++ show index in

    let fvars = if length asG == 0 then replicate (length as) False else map isFreeVar asG in
    let is = [0..length as - 1] in

    let ds = deriveConditionalDomain as asG in
    let ts = deriveConditionalType as asG in

    let allArgs = zip5 fvars as ds ts is in
    let (freeArgs,boundedArgs) = partition (\(b,_,_,_,_) -> b) allArgs in

    -- if predicate contains a constant argument, a bound variable in the goal should be compared to that constant
    let argInit = concat $ map (\(_,a,d,t,i) -> if isConstAexpr a then
                                                    let ai = argPrefix ++ show i in
                                                    [domainToDecl d ++ typeToString False d t i ++ dimPar (typeToDim False d t i) ++ ai ++ "(m); " ++ ai ++ " = " ++ aexprToString show (evalAexpr a) ++ ";"]
                                                else []) freeArgs in
    let argCmp  = concat $ map (\(_,a,d,t,i) -> if isConstAexpr a then
                                                    ["(" ++ argPrefix ++ show i ++ " == " ++ aexprToString show (evalAexpr a) ++ ")"]
                                                else []) boundedArgs in

    let bexpr = foldBool bexpr' in
    let (varMap,crossProductTable) = createCrossProducTable bexpr in
    let openDbConn = if length crossProductTable > 0 then
                         ["uint m = 0;",
                          "uint rv;",
                          "string ds = \"DS1\";",
                          "tdbOpenConnection(ds);"]
                     else []
    in
    let closeDbConn = if length crossProductTable > 0 then
                         ["tdbCloseConnection(ds);"]
                     else []
    in

    let boundedArgMap = M.fromList $ map (\(_,a,d,t,i) -> (a,(d,t,i))) boundedArgs in
    let freeArgMap    = M.fromList $ map (\(_,a,d,t,i) -> (a,(d,t,i))) freeArgs in

    let (_,b,domain,_,body) = bexprToSecreC S.empty boundedArgMap freeArgMap varMap 0 bexpr in
    let finalChoice = [domainToDecl domain ++ "bool [[1]] b = (" ++ intercalate " & " (b:argCmp) ++ ");"] in

    let structDomain = generateTemplateUse (domain == Private) freeArgs in
    let structType   = structName ++ structDomain in

    let funType     = if boolOnly || structType == "" then domainToDecl domain ++ "bool" else structType in
    let funReturn   = if boolOnly || structType == "" then ["return(any(b));"] else generateReturn freeArgs structType in

    let template     = generateTemplateDecl False allArgs in
    let declaration = [--predToString "//" pname as bexpr,
                       template,
                       funType ++ " " ++ funName ++ "(" ++ intercalate ", " (map (\(_,_,d,t,i) -> domainToDecl d ++ typeToString True d t i ++ " " ++ argPrefix ++ show i) boundedArgs) ++ "){"] in

    let footer = ["}\n\n"] in

    (domain, structType, declaration ++ map (indent ++) (openDbConn ++ crossProductTable ++ argInit ++body ++ closeDbConn ++ finalChoice ++ funReturn) ++ footer)

-- create a big cross product table with a map from
-- TODO can be much more efficient if we take into account Primary / Foreign keys
createCrossProducTable :: Formula -> (M.Map Term [String], [String])
createCrossProducTable bexpr =
    let zs = extractAllPredicates bexpr in
    if length zs == 0 then (M.empty, []) else
    let fs = zipWith (\(f,_) i -> f ++ "_" ++ show i) zs [0..length zs - 1] in

    let s1 = ["m = " ++ intercalate " * " (map (\f -> f ++ "_m") fs) ++ ";"] in
    let (colMapData, s0, s2) = processTables 0 [] fs zs in

    let colMap = map (\(x,j) -> (x,[colPrefix ++ show j])) colMapData in
    (M.fromListWith (++) colMap, s0 ++ s1 ++ s2)

processTables :: Int -> [String] -> [String] -> [(String,[Term])] -> ([(Term,Int)],[String],[String])
processTables _ _ _ [] = ([],[],[])
processTables c ms (f:fs) ((tableName,xs):rest) =

    -- old indices
    let is = [0..length xs - 1] in
    -- new indices
    let js = map (c +) is in

    let m = f ++ "_m" in
    let s0 = ["uint " ++ m ++ " = tdbGetRowCount(ds," ++ show tableName ++ ");"] in
    let s1 = ["uint " ++ f ++ "_n = m / (" ++ intercalate " * " (m:ms) ++ ");",
              "uint [[1]] " ++ f ++ "_ms (" ++ f ++ "_m); " ++ f ++ "_ms = 1;",
              "uint [[1]] " ++ f ++ "_ns (" ++ f ++ "_m); " ++ f ++ "_ns = " ++ f ++ "_n;"] in
    let s2 = concat $ zipWith3 (processTableColumn tableName f m) xs is js in

    let (colMapRest, before, after) = processTables (c + length xs) (m:ms) fs rest in
    (zip xs js ++ colMapRest, s0 ++ before, s1 ++ s2 ++ after)

processTableColumn :: String -> String -> String -> Term -> Int -> Int -> [String]
processTableColumn tableName f m x i j =
    let colj = colPrefix ++ show j in
    let s0 = getColumn tableName f i colj m x in
    let s1 = [] in
    s0 ++ s1


-- TODO a Free variable should theoretically result in a dynamic type, but we do not have such constructions yet
getColumn :: String -> String -> Int -> String -> String -> Term -> [String]
getColumn tableName f colIndex varName m arg =
    case arg of
        AVar (Bound Private VarNum  _) -> getIntColumn tableName f colIndex (domainToDecl Private ++ "int32 [[1]]") varName m
        AVar (Bound Private VarText _) -> getStrColumn tableName f colIndex Private varName m
        AVar (Bound Public VarNum  _)  -> getIntColumn tableName f colIndex (domainToDecl Public ++ "uint32 [[1]]") varName m
        AVar (Bound Public VarText _)  -> getStrColumn tableName f colIndex Public varName m
        _                        -> error $ error_complexExpression arg

getIntColumn :: String -> String -> Int -> String -> String -> String -> [String]
getIntColumn tableName f colIndex varDecl varName _ =
    [varDecl ++ " " ++ varName ++ " = getIntColumn(ds," ++ show tableName ++ ", " ++ show colIndex ++ " :: uint, m, " ++ f ++ "_m, " ++ f ++ "_n);"]

getStrColumn :: String -> String -> Int -> DomainType -> String -> String -> [String]
getStrColumn tableName f i d varName m =

    -- domain of the extracted column
    let domain  = domainToDecl d in
    let domain2 = domainToFullDecl Private in
    -- name of the string column
    let varNameStr = strColumn varName in

    -- types of the two result columns, and the temporary variable that holds classified data
    let varType    = typeToString True  d VarText i in
    let varTypeStr = typeToString False d VarText i in

    -- dimension for the hash and the pure string columns
    let varDim     = typeToDim True  d VarText i in
    let varDimStr  = typeToDim False d VarText i in

    let temp = tempVar varName in
    let struct = if d == Private then "strColumnPrivate" else "strColumnPublic" in
    [struct ++ "<" ++ domain2 ++ ">" ++ temp ++ " = getStrColumn(ds, \"" ++ tableName ++ "\", " ++ show i ++ " :: uint, m, " ++ f ++ "_m, " ++ f ++ "_n);",
    domain ++ varType    ++ dimPar varDim    ++ varName    ++ " = " ++ temp ++ "." ++ "col"           ++ ";",
    domain ++ varTypeStr ++ dimPar varDimStr ++ varNameStr ++ " = " ++ temp ++ "." ++ strColumn "col" ++ ";"]

join Private _ = Private
join _ Private = Private
join _  _      = Public

meet Public _ = Public
meet _ Public = Public
meet _  _     = Private


domainToStructDecl Public = ""
domainToStructDecl Private = "D "

domainToDecl Public = ""
domainToDecl Private = "pd_shared3p "

domainToMainDecl Public = ""
domainToMainDecl Private = "pd_shared3p "

domainToFullDecl Public = "public"
domainToFullDecl Private = "pd_shared3p "

declassify Public  Public  = ""
declassify Public  Private = ""
declassify Private Private = ""
declassify Private Public = "declassify"

-- TODO we are not good at doing dynamic typecheck here...
-- rewrite everything using datatype construction
join2 _ x "" = x
join2 _ "" x = x
join2 _ x "T" = x
join2 _ "T" x = x
join2 op x y  = if (x == y) then x else error $ error_typeOp op x y

meet2 x Unknown = x
meet2 Unknown x = x
meet2 x y  = if (x == y) then x else error $ error_typeOp "argument matching" x y

-- TODO let us pass the data type not as a string, but as DataType object
-- this assumes that the expression is "folded", i.e. is in DNF form with grouped AND / OR
bexprToSecreC :: S.Set String -> (M.Map Term (DomainType,DataType,Int)) -> (M.Map Term (DomainType,DataType,Int)) -> (M.Map Term [String]) -> Int -> Formula -> (Int,String,DomainType,S.Set String,[String])
bexprToSecreC dv' boundedArgMap freeArgMap varMap c' bexpr =
    let b = bexprPrefix ++ show c' in
    let c = c' + 1 in
    case bexpr of

        BNary BAnds xs -> processRecNary " & " c b xs
        BNary BOrs  xs -> processRecNary " | " c b xs

        BBinary BAnd x1 x2 -> processRecBinary " & " c b x1 x2
        BBinary BOr  x1 x2 -> processRecBinary " | " c b x1 x2

        BUnary BNot x -> processRecUnary "!" c b x

        BListPred (BPredName pred) as ->
            let c'' = c + length as in
            let bs = map (\z -> bexprPrefix ++ show z) [c..c''-1] in
            let (dt,dv,s2) = foldl (\(t0,dv0,ys0) (a,b,i) ->
                                              let (t1,dv1,ys1) = processArg pred dv0 a b i in
                                              (join t0 t1,dv1,ys0 ++ ys1)
                                   ) (Public, dv', []) $ zip3 as bs [0..] in

            let s3 = [domainToDecl dt ++ "bool [[1]] " ++ b ++ " = (" ++ intercalate " & " bs ++ ");"] in
            (c'',b, dt, dv, s2 ++ s3)

        BBinPred BLT x1 x2  -> processRecBinPred " < " c b x1 x2
        BBinPred BLE x1 x2  -> processRecBinPred " <= " c b x1 x2
        BBinPred BEQ x1 x2  -> processRecBinPred " == " c b x1 x2
        BBinPred BGE x1 x2  -> processRecBinPred " >= " c b x1 x2
        BBinPred BGT x1 x2  -> processRecBinPred " > " c b x1 x2

        BBinPred BAsgn (AVar (Free v)) x  ->
            let (c'',b', ptype, dtype, ys') = aexprToSecreC boundedArgMap c x in
            let (dv, ys) = if S.member v dv' then
                               (dv', [domainToDecl ptype ++ " bool [[1]] " ++ b ++ " = (" ++ v ++ " == " ++ b' ++ ");"])
                           else
                               (S.insert v dv', [domainToDecl ptype ++ dtype ++ " [[1]] " ++ v ++ " = " ++ b' ++ ";",
                                                 domainToDecl ptype ++ " bool [[1]] " ++ b ++ " = " ++ "true;"])
            in (c'',v,  ptype, dv, ys' ++ ys)

        BConstBool v -> let x = case v of {True -> "true"; False -> "false"}
                        in (c', x, Public, dv',[])

    where
          processRecUnary op c b x =
              let (c'',b', ptype, dv, ys) = bexprToSecreC dv' boundedArgMap freeArgMap varMap c x in
              (c'',b,  ptype, dv, ys ++ [domainToDecl ptype ++ " bool [[1]] " ++ b ++ " = " ++ op ++ "(" ++ b' ++ ");"])

          processRecBinPred op c b x1 x2 =
              let (c1,b1,ptype1,dtype1,ys1) = aexprToSecreC boundedArgMap c  x1 in
              let (c2,b2,ptype2,dtype2,ys2) = aexprToSecreC boundedArgMap c1 x2 in
              -- TODO we could check 'dtype1 == dtype2' here
              let ptype = join ptype1 ptype2 in
              let rhs = b1 ++ op ++ b2 in
              (c2, b, ptype, dv', ys1 ++ ys2 ++ [domainToDecl ptype ++ " bool [[1]] " ++ b ++ " = " ++ rhs ++ ";"])

          processRecBinary op c b x1 x2 =
              let (c1,b1,ptype1,dv1,ys1) = bexprToSecreC dv' boundedArgMap freeArgMap varMap c x1 in
              let (c2,b2,ptype2,dv2,ys2) = bexprToSecreC dv' boundedArgMap freeArgMap varMap c1 x2 in
              let ptype = join ptype1 ptype2 in
              let rhs = b1 ++ op ++ b2 in
              (c2, b, ptype, dv2, ys1 ++ ys2 ++ [domainToDecl ptype ++ " bool [[1]] " ++ b ++ " = " ++ rhs ++ ";"])

          processRecNary op c b xs =
              let (c'',bs, ptype, dv, ys) =
                      foldl (\(c0, bs0, ptype0, dv0, ys0) x ->
                              let (c1, b1, ptype1, dv1, ys1) = bexprToSecreC dv0 boundedArgMap freeArgMap varMap c0 x in
                              let ptype = join ptype1 ptype0 in
                              (c1, bs0 ++ [b1], ptype, dv1, ys0 ++ ys1)
                            ) (c,[],Public, dv',[]) xs in
              (c'',b, ptype, dv, ys ++ [domainToDecl ptype ++ " bool [[1]] " ++ b ++ " = (" ++ intercalate op bs ++ ");"])

          processArg pred dv arg b' i =
              let (isConst, domain,varType,x) = getTypeVar True boundedArgMap freeArgMap arg in

              let (z:zs) = if M.member arg varMap then varMap ! arg
                           else error $ error_tableArgNotFound pred arg i
              in
              let (dv0, comp0, decl0) = if S.member x dv || isConst then
                                           (dv,            ["(" ++ x ++ " == " ++ z ++ ")"], [])
                                        else
                                           (S.insert x dv, [], [domainToDecl domain ++ varType ++ " [[1]] " ++ x ++ " = " ++ z ++ ";"])
              in
              -- TODO evaluating x before arg_i helps if a public arg_i is compared with private x
              -- we need to swap the arguments if it is the other direction
              let (dv1, comp1, decl1) = if M.member arg boundedArgMap then
                                                let argName = argPrefix ++ (show . (\(_,_,i) -> i)) (boundedArgMap ! arg) in
                                                (dv0,                  ["(" ++ x ++ " == " ++ argName ++ ")"], [])
                                        else if M.member arg freeArgMap then
                                            let argName = argPrefix ++ (show . (\(_,_,i) -> i)) (freeArgMap ! arg) in
                                            if S.member argName dv0 then
                                                (dv0,                  [getFreeVarCmp arg argName z], [])
                                            else
                                                (S.insert argName dv0, [], [getFreeVarAsgn arg argName z])
                                        else (dv0, [], [])
              in
              let comp2 = map (\z' -> "(" ++ x ++ " == " ++ z' ++ ")") zs in
              let comp = comp0 ++ comp1 ++ comp2 in
              let matchInputTables = [domainToDecl domain ++ "bool [[1]] " ++ b' ++ " = " ++ (if length comp > 0 then  intercalate " & " comp  else "reshape(true,m)") ++ ";"] in
              (domain, dv1, decl0 ++ decl1 ++ matchInputTables)


aexprToSecreC :: (M.Map Term (DomainType,DataType,Int)) -> Int -> Term -> (Int,String,DomainType,String,[String])
aexprToSecreC boundedArgMap c' aexpr =
    let b = aexprPrefix ++ show c' in
    let c = c' + 1 in
    case aexpr of

        ANary ASum xs  -> processRecNary " + " c b xs
        ANary AProd xs -> processRecNary " * " c b xs
        AUnary ANeg x  -> processRecUnary "-" c b x

        ABinary ADiv x1 x2  -> processRecBinary False " / " c b x1 x2
        ABinary AMult x1 x2 -> processRecBinary False " * " c b x1 x2
        ABinary AAdd x1 x2  -> processRecBinary False " + " c b x1 x2
        ABinary ASub x1 x2  -> processRecBinary False " - " c b x1 x2
        ABinary AMin x1 x2  -> processRecBinary True  "min" c b x1 x2
        ABinary AMax x1 x2  -> processRecBinary True  "max" c b x1 x2

        _                  -> let (_, domain, dtype, x) = getTypeVar False boundedArgMap M.empty aexpr in
                              (c', x, domain, dtype, [])
    where
        processRecUnary op c b x =
            let (c'',b', ptype, dtype, ys) = aexprToSecreC boundedArgMap c x in
            (c'',b,  ptype, dtype, ys ++ [domainToDecl ptype ++ dtype ++ " [[1]] " ++ b ++ " = " ++ op ++ "(" ++ b' ++ ");"])

        processRecBinary isPrefixOp op c b x1 x2 =
            let (c1,b1,ptype1,dtype1,ys1) = aexprToSecreC boundedArgMap c x1 in
            let (c2,b2,ptype2,dtype2,ys2) = aexprToSecreC boundedArgMap c1 x2 in
            let ptype = join     ptype1 ptype2 in
            let dtype = join2 op dtype1 dtype2 in
            let rhs = if isPrefixOp then op ++ "(" ++ b1 ++ "," ++ b2 ++ ")" else b1 ++ op ++ b2 in
            (c2, b, ptype, dtype, ys1 ++ ys2 ++ [domainToDecl ptype ++ dtype ++ " [[1]] " ++ b ++ " = " ++ rhs ++ ";"])

        processRecNary op c b xs =
            let (c'',bs, ptype, dtype, ys) =
                    foldl (\(c0, bs0, ptype0, dtype0, ys0) x ->
                               let (c1, b1, ptype1, dtype1, ys1) = aexprToSecreC boundedArgMap c0 x in
                               let ptype = join ptype1 ptype0 in
                               let dtype = if dtype0 == "" || (dtype1 == dtype0) then dtype1
                                           else error $ error_typeOp op dtype1 dtype0
                               in (c1, bs0 ++ [b1], ptype, dtype, ys0 ++ ys1)
                          ) (c,[],Public,"",[]) xs in

            (c'',b, ptype, dtype, ys ++ [domainToDecl ptype ++ dtype ++ " [[1]] " ++ b ++ " = (" ++ intercalate op bs ++ ");"])


getFreeVarCmp :: Term -> String -> String -> String
getFreeVarCmp arg x z =
    case arg of
        AVar (Bound Private VarNum  _) -> "(" ++ x ++ " == " ++ z ++ ");"
        AVar (Bound Private VarText _) -> "(" ++ x ++ " == " ++ strColumn z ++ ");"
        AVar (Bound Public VarNum   _) -> "(" ++ x ++ " == " ++ z ++ ");"
        AVar (Bound Public VarText  _) -> "(" ++ x ++ " == " ++ strColumn z ++ ");"

        AConstNum  _ -> "(" ++ x ++ " == " ++ z ++ ");"
        AConstStr  _ -> "(" ++ x ++ " == " ++ strColumn z ++ ");"

        _           -> error $ error_complexExpression arg

getFreeVarAsgn :: Term -> String -> String -> String
getFreeVarAsgn arg x z =
    case arg of
        AVar (Bound Private VarNum  _) -> domainToDecl Private ++ "int32"     ++ " [[1]] " ++ x ++ " = " ++ z ++ ";"
        AVar (Bound Private VarText _) -> domainToDecl Private ++ "xor_uint8" ++ " [[2]] " ++ x ++ " = " ++ strColumn z ++ ";"
        AVar (Bound Public VarNum   _) -> domainToDecl Public  ++ "int32"     ++ " [[1]] " ++ x ++ " = " ++ z ++ ";"
        AVar (Bound Public VarText  _) -> domainToDecl Public  ++ "uint8"     ++ " [[2]] " ++ x ++ " = " ++ strColumn z ++ ";"

        AConstNum  _ -> domainToDecl Public ++ "int32" ++ " [[1]] " ++ x ++ " = " ++ z ++ ";"
        AConstStr  _ -> domainToDecl Public ++ "uint8" ++ " [[2]] " ++ x ++ " = " ++ strColumn z ++ ";"

        _           -> error $ error_complexExpression arg

getTypeVar :: Bool -> M.Map Term (DomainType,DataType,Int) -> M.Map Term (DomainType,DataType,Int) -> Term -> (Bool, DomainType, String, String)
getTypeVar allowFreeArg boundedArgMap freeArgMap arg =
    case arg of
        AVar (Bound Private VarNum  x) -> (False, Private, "int32", x)
        AVar (Bound Private VarText x) -> (False, Private, "xor_uint32", x)
        AVar (Bound Public VarNum  x)  -> (False, Public,  "int32", x)
        AVar (Bound Public VarText x)  -> (False, Public,  "uint32", x)
        AVar (Free z)            -> if M.member arg boundedArgMap then
                                        let (d,t,i) = boundedArgMap ! arg in
                                        (False, d, typeToString True d t i, argPrefix ++ show i)
                                    else if allowFreeArg && M.member arg freeArgMap then
                                        let (d,t,i) = freeArgMap ! arg in
                                        (False, d, typeToString True d t i, argPrefix ++ show i)
                                    -- TODO we actually want error here, and we need to store a separate map
                                    -- for variables generated in assigments of the form 'X is Y'
                                    --else error $ error_argNotFound arg
                                    else (False, Private, "", z)

        AConstNum  v -> (True, Public, "int32",  show v)
        AConstStr  v -> (True, Public, "uint32", "CRC32(" ++ show v ++ ")")

        _           -> error $ error_complexExpression arg

--------------------------
typeToString :: Bool -> DomainType -> DataType -> Int -> String
typeToString isCRC domain dtype i =
    case dtype of
        VarBool -> "bool"
        VarNum  -> "int32"
        VarText -> case domain of
                       Public  -> if isCRC then "uint32" else "uint8"
                       Private -> if isCRC then "xor_uint32" else "xor_uint8"
        _       -> "T" ++ show i

typeToDim :: Bool -> DomainType -> DataType -> Int -> String
typeToDim isCRC domain dtype i =
    case dtype of
        VarBool -> "1"
        VarNum  -> "1"
        VarText -> case domain of
                       Public  -> if isCRC then "1" else "2"
                       Private -> if isCRC then "1" else "2"
        _       -> "N" ++ show i

--------------------------
-- is the term ground (i.e. does not contain any free variables)?
isGround :: AExpr DBVar -> Bool
isGround aexpr =
    case aexpr of
        AVar x -> case x of
                      Free _ -> False
                      _      -> True

        AConstNum  _ -> True
        AConstStr  _ -> True

        ANary _ xs      -> foldl (&&) True $ map processRec xs
        AUnary _ x      -> processRec x
        ABinary _ x1 x2 -> (processRec x1) && (processRec x2)

    where processRec x = isGround x

deriveDomain :: AExpr DBVar -> DomainType
deriveDomain aexpr =
    case aexpr of
        AVar x -> case x of
                      Bound domain _ _ -> domain
                      -- if the type is not known in advance, it is always safe to take public
                      Free _           -> Private

        AConstNum  _ -> Public
        AConstStr  _ -> Public

        ANary _ xs      -> foldl join Public $ map processRec xs
        AUnary _ x      -> processRec x
        ABinary _ x1 x2 -> join (processRec x1) (processRec x2)

    where processRec x = deriveDomain x

deriveType :: AExpr DBVar -> DataType
deriveType aexpr =
    case aexpr of
        AVar x -> fromMaybe Unknown $ dataType x
        AConstNum  _ -> VarNum
        AConstStr  _ -> VarText
        ANary ASum xs  -> VarNum
        ANary AProd xs -> VarNum
        AUnary ANeg x -> VarNum
        ABinary ADiv x1 x2  -> VarNum
        ABinary AMult x1 x2 -> VarNum
        ABinary AAdd x1 x2  -> VarNum
        ABinary ASub x1 x2  -> VarNum
        ABinary AMin x1 x2  -> VarNum
        ABinary AMax x1 x2  -> VarNum

deriveConditionalDomain :: [Term] -> [Term] -> [DomainType]
deriveConditionalDomain asF [] = map deriveDomain asF
deriveConditionalDomain (aF:asF) (aG:asG) =
    let dF = deriveDomain aF in
    let dG = deriveDomain aG in
    (meet dF dG) : (deriveConditionalDomain asF asG)

deriveConditionalType :: [Term] -> [Term] -> [DataType]
deriveConditionalType asF [] = map deriveType asF
deriveConditionalType (aF:asF) (aG:asG) =
    let dF = deriveType aF in
    let dG = deriveType aG in
    (meet2 dF dG) : (deriveConditionalType asF asG)

------------------------------------------------------------------------------------
isFreeVar :: AExpr DBVar -> Bool
isFreeVar aexpr =
    case aexpr of
        AVar x -> isFree x
        _      -> False
