module SecreC where

---------------------------------------------------------
---- Transformation of intermediate representation
----  to SecreC
---------------------------------------------------------

import Data.Hashable
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr
import ErrorMsg
import Rule

indent = "    "
boolPrefix = "b_"
colPrefix  = "col_"
argPrefix  = "arg_"
goalPrefix = "goal_"

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
               "}"]

generateGoal :: [DomainType] -> Maybe (String,[Arg]) -> [String]
generateGoal ds goal =
    case goal of
        Just (goalPname, goalArgs) ->

            let js = [0..length ds-1] in
            let bs = map (\j -> boolPrefix ++ show j) js in
            let (args,declarations) = unzip $ concat $ zipWith generateGoalArg goalArgs [0..length goalArgs-1] in
            ["void main(){"]
               ++ map (indent ++) declarations
               ++ zipWith3 (\b d j -> indent ++ domainToMainDecl d ++ "bool " ++ b ++ " = " ++ goalPrefix ++ goalPname ++ "_" ++ show j ++ "(" ++ intercalate "," args ++ ");") bs ds js
               ++ [indent ++ "print(declassify(" ++ intercalate " | " bs ++ "));", "}"]

        _ -> defaultGoal

generateGoalArg :: Arg -> Int -> [(String,String)]
generateGoalArg arg i =
    let argName = argPrefix ++ show i in
    case arg of

        -- a constant is hard-coded
        AConstBool v -> [(argName, domainToMainDecl Public ++ "bool "   ++ argName ++ " = " ++ case v of {True -> "true"; False -> "false"} ++ ";")]
        AConstNum  v -> [(argName, domainToMainDecl Public ++ "bool "   ++ argName ++ " = " ++ show v ++ ";")]
        AConstStr  v -> [(argName, domainToMainDecl Public ++ "uint32 " ++ argName ++ " = declassify(CRC32(bl_str(" ++ v ++ ") :: pd_shared3p xor_uint8));")]

        -- a bounded argument comes as an input
        AVar (Bound domain  VarNum  x) -> [(argName, domainToMainDecl domain  ++ "int32 "      ++ argName ++ " = argument(" ++ show x ++ ");")]
        AVar (Bound Private VarText x) -> [(argName, domainToMainDecl Private ++ "xor_uint32 " ++ argName ++ " = CRC32(bl_str(argument(" ++ show x ++ ")) :: pd_shared3p xor_uint8);")]
        AVar (Bound Public  VarText x) -> [(argName, domainToMainDecl Public  ++ "uint32 "     ++ argName ++ " = declassify(CRC32(bl_str(argument(" ++ show x ++ ")) :: pd_shared3p xor_uint8));")]

        -- a free argument can be evaluated to anything and is not instantiated
        AVar (Free z) -> []

-- a SecreC program is a list of code lines
-- if no particular goal is given, then we do not create a main statement
generateSecreCscript :: (M.Map PName PMap) -> [RHS] -> String
generateSecreCscript predMap goals =

    -- TODO think whether we want to support more expressions in a goal
    let goal = if length goals > 0 then
                      case head goals of
                          Fact goalPname goalArgs -> Just (goalPname, goalArgs)
                          _                       -> Nothing
               else
                      Nothing
    in
    trace ("G: " ++ show goal) $
    let header = defaultHeader in
    let (ds,body) = unzip $ concat $ M.mapWithKey (createSecreCFuns goal) predMap in
    let mainFun = generateGoal ds goal in

    intercalate "\n" $ header ++ concat body ++ mainFun

createSecreCFuns :: Maybe (PName, [Arg]) -> PName -> PMap -> [(DomainType, [String])]
createSecreCFuns goal pname pmap =
    let unnecessaryFun = case goal of {Just (goalPname, _) -> pname /= goalPname; _ -> False} in
    if unnecessaryFun then [] else
    let (keys,values) = unzip (M.toList pmap) in
    let is = [0..length keys - 1] in
    let goalArgs = case goal of {Just (_, goalArgs) -> goalArgs; _ -> []} in
    zipWith3 (createSecreCFun pname goalArgs) is keys values

createSecreCFun :: PName -> [Arg] -> Int -> [Arg] -> Arg -> (DomainType, [String])
createSecreCFun pname asG index as bexpr =
    --if not(isGround bexpr) then [] else
    let fvars = if length asG == 0 then replicate (length as) False else map isFreeVar asG in
    let is = [0..length as - 1] in

    let ds = deriveConditionalDomain as asG in
    let ts = deriveConditionalType as asG in

    let allArgs = zip5 fvars as ds ts is in
    let (freeArgs,boundedArgs) = partition (\(b,_,_,_,_) -> b) allArgs in

    -- if predicate contains a constant argument, a bound variable in the goal should be compared to that constant
    --let argInit = concat $ map (\(_,a,d,t,i) -> if isConstTerm a then
    --                                                [indent ++ domainToDecl d ++ typeToString d t i ++ " " ++ argPrefix ++ show i ++ " = " ++ aexprToString (evalAexpr a) ++ ";"]
    --                                            else []) freeArgs in
    let argCmp  = concat $ map (\(_,a,d,t,i) -> if isConstTerm a then
                                                    ["(" ++ argPrefix ++ show i ++ " == " ++ aexprToString (evalAexpr a) ++ ")"]
                                                else []) boundedArgs in

    let aexpr = foldBool bexpr in
    let (varMap,crossProductTable) = createCrossProducTable aexpr in
    let openDbConn = if length crossProductTable > 0 then
                         [indent ++ "uint m = 0;",
                          indent ++ "uint rv;",
                          indent ++ "string ds = \"DS1\";",
                          indent ++ "tdbOpenConnection(ds);"]
                     else []
    in
    let closeDbConn = if length crossProductTable > 0 then
                         [indent ++ "tdbCloseConnection(ds);"]
                     else []
    in

    let argMap = M.fromList $ map (\(_,a,d,t,i) -> (a,(d,t,i))) boundedArgs in
    let (_,b,domain,_,_,body) = aexprToSecreC S.empty argMap varMap 0 aexpr in

    -- we need a domain template D only if private variables are used at all
    let templateDomain = if length crossProductTable > 0 || elem Private ds then ["domain D "]
                         else [] in
    let template = templateDomain ++ (map (\(_,_,_,_,i) -> "type T" ++ show i) $ filter (\(_,_,_,t,_) -> t == Unknown) boundedArgs) in

    let declaration = [predToString "//" pname as bexpr,
                       if length template > 0 then "template <" ++ intercalate ", " template ++ ">" else "",
                       domainToDecl domain ++ "bool " ++ goalPrefix ++ pname ++ "_" ++ show index ++ "(" ++ intercalate ", " (map (\(_,_,d,t,i) -> domainToDecl d ++ typeToString d t i ++ " " ++ argPrefix ++ show i) boundedArgs) ++ "){"] in

    let footer = [indent ++ "return any(" ++ intercalate " & " (b:argCmp) ++ ");",
                  "}\n\n"] in
    (domain, declaration ++ openDbConn ++ (map (indent ++) crossProductTable) ++ (map (indent ++) body) ++ closeDbConn ++ footer)

-- create a big cross product table with a map from 
-- TODO can be much more efficient if we take into account Primary / Foreign keys
createCrossProducTable :: AExpr Var -> (M.Map Arg [String], [String])
createCrossProducTable aexpr =
    let zs = extractAllPredicates aexpr in
    if length zs == 0 then (M.empty, []) else
    let fs = zipWith (\(f,_) i -> f ++ "_" ++ show i) zs [0..length zs - 1] in

    let s1 = ["m = " ++ intercalate " * " (map (\f -> f ++ "_m") fs) ++ ";"] in
    let (colMapData, s0, s2) = processTables 0 [] fs zs in

    let colMap = map (\(x,j) -> (x,[colPrefix ++ show j])) colMapData in
    (M.fromListWith (++) colMap, s0 ++ s1 ++ s2)

processTables :: Int -> [String] -> [String] -> [(String,[Arg])] -> ([(Arg,Int)],[String],[String])
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

processTableColumn :: String -> String -> String -> Arg -> Int -> Int -> [String]
processTableColumn tableName f m x i j =
    let colj = colPrefix ++ show j in
    let s0 = getColumn tableName i colj m x in
    let s1 = [colj ++ " = copyBlock(myReplicate(" ++ colj ++ ", " ++ f ++ "_ms, " ++ f ++ "_ns), {" ++ f ++ "_m * " ++ f ++ "_n}, {m / (" ++ f ++ "_m * " ++ f ++ "_n)});"] in
    s0 ++ s1


-- TODO a Free variable should theoretically result in a dynamic type, but we do not have such constructions yet
getColumn :: String -> Int -> String -> String -> Arg -> [String]
getColumn tableName colIndex varName m arg =
    case arg of
        AVar (Bound Private VarNum  _) -> getIntColumn tableName colIndex (domainToDecl Private ++ "int32 [[1]]") varName m
        AVar (Bound Private VarText _) -> getStrColumn tableName colIndex "" (domainToDecl Private ++ "xor_uint32 [[1]]") varName m
        AVar (Bound Public VarNum  _)  -> getIntColumn tableName colIndex (domainToDecl Public ++ "uint32 [[1]]") varName m
        AVar (Bound Public VarText _)  -> getStrColumn tableName colIndex "declassify" (domainToDecl Public ++ "uint32 [[1]]") varName m
        _                        -> error $ error_complexExpression arg

getIntColumn :: String -> Int -> String -> String -> String -> [String]
getIntColumn tableName colIndex varDecl varName _ =
    [varDecl ++ " " ++ varName ++ " = tdbReadColumn(ds," ++ show tableName ++ ", " ++ show colIndex ++ " :: uint);"]

getStrColumn :: String -> Int -> String -> String -> String -> String -> [String]
getStrColumn tableName colIndex declassify varDecl varName m =
    [varDecl ++ " " ++ varName ++ " = reshape(0," ++ m ++ ");",
     "rv = tdbReadColumn(ds," ++ show tableName ++ ", " ++ show colIndex ++ " :: uint);",
     "for (uint i = 0; i < " ++ m ++ "; i++){",
     indent ++ domainToDecl Private ++ "xor_uint8 [[1]] temp = tdbVmapGetVlenValue(rv, \"values\", i);",
     indent ++ varName ++ "[i] = " ++ declassify ++ "(CRC32(temp));",
     "}"]

join Private _ = Private
join _ Private = Private
join _  _      = Public

meet Public _ = Public
meet _ Public = Public
meet _  _     = Private

domainToDecl Public = ""
domainToDecl Private = "D "

domainToMainDecl Public = ""
domainToMainDecl Private = "pd_shared3p "

-- TODO we are not good at doing dynamic typecheck here...
-- rewrite everything using datatype construction
join2 _ x "" = x
join2 _ "" x = x
join2 _ x "T" = x
join2 _ "T" x = x
join2 op x y  = if (x == y) then x else error $ error_typeOp op x y

meet2 x Unknown = Unknown
meet2 Unknown x = x
meet2 x y  = if (x == y) then x else error $ error_typeOp "argument matching" x y

-- this assumes that the expression is "folded", i.e. is in DNF form with grouped AND / OR
aexprToSecreC :: S.Set VName -> (M.Map Arg (DomainType,DataType,Int)) -> (M.Map Arg [String]) -> Int -> AExpr Var -> (Int,String,DomainType,String,S.Set VName,[String])
aexprToSecreC declaredVars argMap varMap c' aexpr =
    let b = boolPrefix ++ show c' in
    let c = c' + 1 in
    case aexpr of

        ANary (AMember pred) as -> let c'' = c + length as in
                                   let bs = map (\z -> boolPrefix ++ show z) [c..c''-1] in
                                   let (dt,dv,s2) = foldl (\(t0,dv0,ys0) (a,b,i) ->
                                                               let (t1,dv1,ys1) = processArg pred dv0 a b i in
                                                               (join t0 t1,dv1,ys0 ++ ys1)
                                                          ) (Public,declaredVars, []) $ zip3 as bs [0..] in

                                   let s3 = [domainToDecl dt ++ "bool [[1]] " ++ b ++ " = (" ++ intercalate " & " bs ++ ");"] in
                                   (c'',b, dt,"bool",dv, s2 ++ s3)

        ANary AAnds xs -> processRecNary " & " c b xs
        ANary AOrs  xs -> processRecNary " | " c b xs
        ANary ASum xs  -> processRecNary " + " c b xs
        ANary AProd xs -> processRecNary " * " c b xs

        AUnary ANot x -> processRecUnary "!" c b x
        AUnary ANeg x -> processRecUnary "-" c b x

        ABinary ADiv x1 x2  -> processRecBinary False "" " / " c b x1 x2
        ABinary AMult x1 x2 -> processRecBinary False "" " * " c b x1 x2
        ABinary AAdd x1 x2  -> processRecBinary False "" " + " c b x1 x2
        ABinary ASub x1 x2  -> processRecBinary False "" " - " c b x1 x2
        ABinary AMin x1 x2  -> processRecBinary True  "" "min" c b x1 x2
        ABinary AMax x1 x2  -> processRecBinary True  "" "max" c b x1 x2

        ABinary AAnd x1 x2 -> processRecBinary False "" " & " c b x1 x2
        ABinary AOr  x1 x2 -> processRecBinary False "" " | " c b x1 x2
        ABinary ALT x1 x2  -> processRecBinary False "bool" " < " c b x1 x2
        ABinary ALE x1 x2  -> processRecBinary False "bool" " <= " c b x1 x2
        ABinary AEQ x1 x2  -> processRecBinary False "bool" " == " c b x1 x2
        ABinary AGE x1 x2  -> processRecBinary False "bool" " >= " c b x1 x2
        ABinary AGT x1 x2  -> processRecBinary False "bool" " > " c b x1 x2

        _                  -> let (_, domain, dtype, x) = getTypeVar argMap aexpr in
                              (c', x, domain, dtype, declaredVars,[])


    where
          processRecUnary op c b x      = let (c'',b', ptype, dtype, declaredVars', ys) = aexprToSecreC declaredVars argMap varMap c x in
                                              (c'',b,  ptype, dtype, declaredVars', ys ++ [domainToDecl ptype ++ dtype ++ " [[1]] " ++ b ++ " = " ++ op ++ "(" ++ b' ++ ");"])

          processRecBinary isPrefixOp dtype0 op c b x1 x2 = let (c1,b1,ptype1,dtype1,dv1,ys1) = aexprToSecreC declaredVars argMap varMap c x1 in
                                          let (c2,b2,ptype2,dtype2,dv2,ys2) = aexprToSecreC dv1 argMap varMap c1 x2 in
                                          let ptype = join ptype1 ptype2 in
                                          let dtype = if dtype0 /= "" then dtype0 else join2 op dtype1 dtype2 in
                                          let rhs = if isPrefixOp then op ++ "(" ++ b1 ++ "," ++ b2 ++ ")" else b1 ++ op ++ b2 in
                                          (c2, b, ptype, dtype, dv2, ys1 ++ ys2 ++ [domainToDecl ptype ++ dtype ++ " [[1]] " ++ b ++ " = " ++ rhs ++ ";"])

          processRecNary op c b xs  = let (c'',bs, ptype, dtype, dv, ys) = foldl (\(c0, bs0, ptype0, dtype0, dv0, ys0) x ->
                                                                                 let (c1, b1, ptype1, dtype1, dv1, ys1) = aexprToSecreC dv0 argMap varMap c0 x in
                                                                                 let ptype = join ptype1 ptype0 in
                                                                                 let dtype = if dtype0 == "" || (dtype1 == dtype0) then dtype1
                                                                                             else error $ error_typeOp op dtype1 dtype0 in
                                                                                 (c1, bs0 ++ [b1], ptype, dtype, dv1, ys0 ++ ys1)
                                                                             ) (c,[],Public,"",S.empty,[]) xs in

                                      (c'',b, ptype, dtype, dv, ys ++ [domainToDecl ptype ++ dtype ++ " [[1]] " ++ b ++ " = (" ++ intercalate op bs ++ ");"])

          processArg pred dv arg b' i =
              let (isConst, domain,varType,x) = getTypeVar argMap arg in

              let (z:zs) = if M.member arg varMap then varMap ! arg
                           else error $ error_tableArgNotFound pred arg i
              in
              let (dv', comp0, decl) = if S.member x dv || isConst then
                                           (dv,            ["(" ++ z ++ " == " ++ x ++ ")"], [])
                                       else
                                           (S.insert x dv, [], [domainToDecl domain ++ varType ++ " [[1]] " ++ x ++ " = " ++ z ++ ";"])
              in
              let comp1 = map (\z' -> "(" ++ z' ++ " == " ++ x ++ ")") zs in
              -- TODO evaluating x before arg_i helps if a public arg_i is compared with private x
              -- we need to swap the arguments if it is the other direction
              let comp2 = if M.member arg argMap then ["(" ++ x ++ " == " ++ argPrefix ++ (show . (\(_,_,i) -> i)) (argMap ! arg) ++ ")"] else [] in
              let comp = comp0 ++ comp1 ++ comp2 in
              let matchInputTables = [domainToDecl domain ++ "bool [[1]] " ++ b' ++ " = " ++ (if length comp > 0 then  intercalate " & " comp  else "reshape(true,m)") ++ ";"] in
              (domain, dv', decl ++ matchInputTables)

getTypeVar :: (M.Map Arg (DomainType,DataType,Int)) -> Arg -> (Bool, DomainType, String, String)
getTypeVar argMap arg =
    case arg of
        AVar (Bound Private VarNum  x) -> (False, Private, "int32", x)
        AVar (Bound Private VarText x) -> (False, Private, "xor_uint32", x)
        AVar (Bound Public VarNum  x)  -> (False, Public,  "int32", x)
        AVar (Bound Public VarText x)  -> (False, Public,  "uint32", x)
        AVar (Free z)            -> let (d,t,i) = if M.member arg argMap then argMap ! arg
                                            else error $ error_argNotFound arg
                                    in
                                    (False, d, typeToString d t i, argPrefix ++ show i)

        AConstBool v -> (True, Public, "bool",   case v of {True -> "true"; False -> "false"})
        AConstNum  v -> (True, Public, "int32",  show v)
        AConstStr  v -> (True, Public, "uint32", "CRC32(" ++ show v ++ ")")

        _           -> error $ error_complexExpression arg

--------------------------
typeToString :: DomainType -> DataType -> Int -> String
typeToString domain dtype i =
    case dtype of
        VarBool -> "bool"
        VarNum  -> "int32"
        VarText -> case domain of
                       Public  -> "uint32"
                       Private -> "xor_uint32"
        _       -> "T" ++ show i

--------------------------
-- is the term ground (i.e. does not contain any free variables)?
isGround :: AExpr Var -> Bool
isGround aexpr =
    case aexpr of
        AVar x -> case x of
                      Free _ -> False
                      _      -> True

        AConstBool _ -> True
        AConstNum  _ -> True
        AConstStr  _ -> True

        ANary _ xs      -> foldl (&&) True $ map processRec xs
        AUnary _ x      -> processRec x
        ABinary _ x1 x2 -> (processRec x1) && (processRec x2)

    where processRec x = isGround x

deriveDomain :: AExpr Var -> DomainType
deriveDomain aexpr =
    case aexpr of
        AVar x -> case x of
                      Bound domain _ _ -> domain
                      -- if the type is not known in advance, it is always safe to take public
                      Free _           -> Private

        AConstBool _ -> Public
        AConstNum  _ -> Public
        AConstStr  _ -> Public

        ANary _ xs      -> foldl join Public $ map processRec xs
        AUnary _ x      -> processRec x
        ABinary _ x1 x2 -> join (processRec x1) (processRec x2)

    where processRec x = deriveDomain x

deriveType :: AExpr Var -> DataType
deriveType aexpr =
    case aexpr of
        AVar x -> case x of
                      Bound _ dtype _ -> dtype
                      Free _          -> Unknown

        AConstBool _ -> VarBool
        AConstNum  _ -> VarNum
        AConstStr  _ -> VarText

        ANary AAnds xs -> VarBool
        ANary AOrs  xs -> VarBool
        ANary ASum xs  -> VarNum
        ANary AProd xs -> VarNum

        AUnary ANot x -> VarBool
        AUnary ANeg x -> VarNum

        ABinary ADiv x1 x2  -> VarNum
        ABinary AMult x1 x2 -> VarNum
        ABinary AAdd x1 x2  -> VarNum
        ABinary ASub x1 x2  -> VarNum
        ABinary AMin x1 x2  -> VarNum
        ABinary AMax x1 x2  -> VarNum

        ABinary AAnd x1 x2 -> VarBool
        ABinary AOr  x1 x2 -> VarBool
        ABinary ALT x1 x2  -> VarBool
        ABinary ALE x1 x2  -> VarBool
        ABinary AEQ x1 x2  -> VarBool
        ABinary AGE x1 x2  -> VarBool
        ABinary AGT x1 x2  -> VarBool

deriveConditionalDomain :: [Arg] -> [Arg] -> [DomainType]
deriveConditionalDomain asF [] = map deriveDomain asF
deriveConditionalDomain (aF:asF) (aG:asG) =
    let dF = deriveDomain aF in
    let dG = deriveDomain aG in
    (meet dF dG) : (deriveConditionalDomain asF asG)

deriveConditionalType :: [Arg] -> [Arg] -> [DataType]
deriveConditionalType asF [] = map deriveType asF
deriveConditionalType (aF:asF) (aG:asG) =
    let dF = deriveType aF in
    let dG = deriveType aG in
    (meet2 dF dG) : (deriveConditionalType asF asG)

------------------------------------------------------------------------------------
isFreeVar :: AExpr Var -> Bool
isFreeVar aexpr =
    case aexpr of
        AVar x -> case x of
                      Free _ -> True
                      _      -> False
        _      -> False

