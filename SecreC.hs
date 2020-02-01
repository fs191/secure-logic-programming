module SecreC where

---------------------------------------------------------
---- Transformation of intermediate representation
----  to SecreC
---------------------------------------------------------

import Data.Hashable
import Data.List
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr
import ErrorMsg
import Rule

indent = "    "
nv0 = "b"
colPrefix = "col_"

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
               indent ++ "pd_shared3p uint32 dummy;",
               indent ++ "//TODO: state your own goal here",
               "}"]

-- a SecreC program is a list of code lines
-- if no particular goal is given, then we do not create a main statement
generateSecreCscript :: (M.Map PName PMap) -> [RHS] -> String
generateSecreCscript predMap goals =
    let header = defaultHeader in
    let body = concat $ M.mapWithKey createSecreCFuns predMap in
    -- TODO add goal processing here
    let goal = defaultGoal in
    intercalate "\n" $ header ++ body ++ defaultGoal

createSecreCFuns :: PName -> PMap -> [String]
createSecreCFuns pname pmap =
    let (keys,values) = unzip (M.toList pmap) in
    let is = [0..length keys - 1] in
    concat $ zipWith3 (createSecreCFun pname) is keys values

createSecreCFun :: PName -> Int -> [Arg] -> Arg -> [String]
createSecreCFun pname index as bexpr =
    --if not(isGround bexpr) then [] else
    let is = [0..length as - 1] in
    let declaration = [predToString "//" pname as bexpr,
                       "template <domain D, " ++ intercalate ", " (map (\i -> "type T" ++ show i) is) ++ ">",
                       "D bool goal_" ++ pname ++ "_" ++ show index ++ "(" ++ intercalate ", " (map (\i -> "D T" ++ show i ++ " arg" ++ show i) is) ++ "){"] in

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
    let argMap = M.fromList $ zip as is in

    let (_,b,_,_,_,body) = aexprToSecreC S.empty argMap varMap 0 aexpr in
    let footer = [indent ++ "return any(" ++ b ++ ");",
                  "}\n\n"] in
    declaration ++ openDbConn ++ (map (indent ++) crossProductTable) ++ (map (indent ++) body) ++ closeDbConn ++ footer

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
        AVar (Private VarNum  _) -> getIntColumn tableName colIndex "D int32 [[1]]" varName m
        AVar (Private VarText _) -> getStrColumn tableName colIndex "" "D xor_uint32 [[1]]"varName m
        AVar (Public VarNum  _)  -> getIntColumn tableName colIndex "uint32 [[1]]" varName m
        AVar (Public VarText _)  -> getStrColumn tableName colIndex "declassify" "uint32 [[1]]"varName m
        _                        -> error $ error_complexExpression arg

getIntColumn :: String -> Int -> String -> String -> String -> [String]
getIntColumn tableName colIndex varDecl varName _ =
    [varDecl ++ " " ++ varName ++ " = tdbReadColumn(ds," ++ show tableName ++ ", " ++ show colIndex ++ " :: uint);"]

getStrColumn :: String -> Int -> String -> String -> String -> String -> [String]
getStrColumn tableName colIndex declassify varDecl varName m =
    [varDecl ++ " " ++ varName ++ " = reshape(0," ++ m ++ ");",
     "rv = tdbReadColumn(ds," ++ show tableName ++ ", " ++ show colIndex ++ " :: uint);",
     "for (uint i = 0; i < " ++ m ++ "; i++){",
     indent ++ "D xor_uint8 [[1]] temp = tdbVmapGetVlenValue(rv, \"values\", i);",
     indent ++ varName ++ "[i] = " ++ declassify ++ "(CRC32(temp));",
     "}"]

-- TODO use nice construction for privacy types
join "D" _ = "D"
join _ "D" = "D"
join _  _  = ""

-- TODO we are not good at doing dynamic typecheck here...
join2 _ x "" = x
join2 _ "" x = x
join2 _ x "T" = x
join2 _ "T" x = x
join2 op x y  = if (x == y) then x else error $ error_typeOp op x y

-- this assumes that the expression is "folded", i.e. is in DNF form with grouped AND / OR
aexprToSecreC :: S.Set VName -> (M.Map Arg Int) -> (M.Map Arg [String]) -> Int -> AExpr Var -> (Int,String,String,String,S.Set VName,[String])
aexprToSecreC declaredVars argMap varMap c' aexpr =
    let b = nv0 ++ show c' in
    let c = c' + 1 in
    case aexpr of

        ANary (AMember pred) as -> let c'' = c + length as in
                                   let bs = map (\z -> nv0 ++ show z) [c..c''-1] in
                                   let (dt,dv,s2) = foldl (\(t0,dv0,ys0) (a,b,i) ->
                                                               let (t1,dv1,ys1) = processArg pred dv0 a b i in
                                                               (join t0 t1,dv1,ys0 ++ ys1)
                                                          ) ("",declaredVars, []) $ zip3 as bs [0..] in

                                   let s3 = ["D bool [[1]] " ++ b ++ " = (" ++ intercalate " & " bs ++ ");"] in
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
                                              (c'',b,  ptype, dtype, declaredVars', ys ++ [ptype ++ " " ++ dtype ++ " [[1]] " ++ b ++ " = " ++ op ++ "(" ++ b' ++ ");"])

          processRecBinary isPrefixOp dtype0 op c b x1 x2 = let (c1,b1,ptype1,dtype1,dv1,ys1) = aexprToSecreC declaredVars argMap varMap c x1 in
                                          let (c2,b2,ptype2,dtype2,dv2,ys2) = aexprToSecreC dv1 argMap varMap c1 x2 in
                                          let ptype = join ptype1 ptype2 in
                                          let dtype = if dtype0 /= "" then dtype0 else join2 op dtype1 dtype2 in
                                          let rhs = if isPrefixOp then op ++ "(" ++ b1 ++ "," ++ b2 ++ ")" else b1 ++ op ++ b2 in
                                          (c2, b, ptype, dtype, dv2, ys1 ++ ys2 ++ [ptype ++ " " ++ dtype ++ " [[1]] " ++ b ++ " = " ++ rhs ++ ";"])

          processRecNary op c b xs  = let (c'',bs, ptype, dtype, dv, ys) = foldl (\(c0, bs0, ptype0, dtype0, dv0, ys0) x ->
                                                                                 let (c1, b1, ptype1, dtype1, dv1, ys1) = aexprToSecreC dv0 argMap varMap c0 x in
                                                                                 let ptype = join ptype1 ptype0 in
                                                                                 let dtype = if dtype0 == "" || (dtype1 == dtype0) then dtype1
                                                                                             else error $ error_typeOp op dtype1 dtype0 in
                                                                                 (c1, bs0 ++ [b1], ptype, dtype, dv1, ys0 ++ ys1)
                                                                             ) (c,[],"","",S.empty,[]) xs in

                                      (c'',b, ptype, dtype, dv, ys ++ [ptype ++ " " ++ dtype ++ " [[1]] " ++ b ++ " = (" ++ intercalate op bs ++ ");"])

          processArg pred dv arg b' i =
              let (isConst, domain,varType,x) = getTypeVar argMap arg in

              let (z:zs) = if M.member arg varMap then varMap ! arg
                           else error $ error_tableArgNotFound pred arg i
              in
              let (dv', comp0, decl) = if S.member x dv || isConst then
                                           (dv,            ["(" ++ z ++ " == " ++ x ++ ")"], [])
                                       else
                                           (S.insert x dv, [], [domain ++ " " ++ varType ++ " [[1]] " ++ x ++ " = " ++ z ++ ";"])
              in
              let comp1 = map (\z' -> "(" ++ z' ++ " == " ++ x ++ ")") zs in
              let comp2 = if M.member arg argMap then ["(arg" ++ show (argMap ! arg) ++ " == " ++ x ++ ")"] else [] in
              let comp = comp0 ++ comp1 ++ comp2 in
              let matchInputTables = ["D bool [[1]] " ++ b' ++ " = " ++ (if length comp > 0 then  intercalate " & " comp  else "reshape(true,m)") ++ ";"] in
              ("D", dv', decl ++ matchInputTables)

getTypeVar :: (M.Map Arg Int) -> Arg -> (Bool, String,String,String)
getTypeVar argMap arg =
    case arg of
        AVar (Private VarNum  x) -> (False, "D", "int32", x)
        AVar (Private VarText x) -> (False, "D", "xor_uint32", x)
        AVar (Public VarNum  x)  -> (False, "",  "int32", x)
        AVar (Public VarText x)  -> (False, "",  "uint32", x)
        AVar (Free z)            -> let i = if M.member arg argMap then argMap ! arg
                                            else error $ error_argNotFound arg
                                    in
                                    (False, "D",  "T" ++ show i, "arg" ++ show i)

        AConstBool v -> (True, "", "bool",   case v of {True -> "true"; False -> "false"})
        AConstNum  v -> (True, "", "int32",  show v)
        AConstStr  v -> (True, "", "uint32", "CRC32(" ++ show v ++ ")")

        _           -> error $ error_complexExpression arg

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


