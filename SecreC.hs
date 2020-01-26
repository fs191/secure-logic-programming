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

createHeader = [
    -- import essentials
    "import stdlib;",
    "import shared3p;",
    "import shared3p_string;",
    "import shared3p_table_database;",
    "import table_database;\n",
    "import aux;\n",
    "domain pd_shared3p shared3p;\n"]

-- a SecreC program is a list of code lines
-- if no particular goal is given, then we do not create a main statement
createSecreCNoGoal :: (M.Map PName PMap) -> [String]
createSecreCNoGoal predMap =
    let header = createHeader in
    let body = concat $ M.mapWithKey createSecreCFuns predMap in
    header ++ body

createSecreCFuns :: PName -> PMap -> [String]
createSecreCFuns pname pmap = concat $ M.mapWithKey (createSecreCFun pname) pmap

-- TODO add fresh indices to the function names
createSecreCFun :: PName -> [Arg] -> Arg -> [String]
createSecreCFun pname as bexpr =
    --if not(isGround bexpr) then [] else
    let declaration = [predToString "//" pname as bexpr,
                       "template <domain D, type T>",
                       "D bool _goal_" ++ pname ++ "(D T [[1]] args){",
                       indent ++ "uint rv;",

                       indent ++ "int32 [[1]] temp_int32;",
                       indent ++ "uint32 [[1]] temp_uint32;",
                       indent ++ "bool [[1]] temp_bool;",

                       indent ++ "D int32 [[1]] temp_D_int32;",
                       indent ++ "D xor_uint32 [[1]] temp_D_xor_uint32;",
                       indent ++ "D bool [[1]] temp_D_bool;",

                       indent ++ "string ds = \"DS1\";",
                       indent ++ "tdbOpenConnection(ds);"] in

    let aexpr = foldBool bexpr in
    let (varMap,crossProductTable) = createCrossProducTable aexpr in
    let argMap = M.fromList $ zip as [0..] in

    let (_,b,_,_,_,body) = aexprToSecreC S.empty argMap varMap 0 aexpr in
    let footer = [indent ++ "tdbCloseConnection(ds);",
                  indent ++ "return any(" ++ b ++ ");",
                  "}\n\n"] in
    declaration ++ (map (indent ++) crossProductTable) ++ (map (indent ++) body) ++ footer

-- create a big cross product table with a map from 
-- TODO can be much more efficient if we take into account Primary / Foreign keys
createCrossProducTable :: AExpr Var -> (M.Map Arg [String], [String])
createCrossProducTable aexpr =
    let zs = extractAllPredicates aexpr in
    if length zs == 0 then (M.empty, ["uint m = 0;"]) else
    let fs = zipWith (\(f,_) i -> f ++ "_" ++ show i) zs [0..length zs - 1] in

    let s1 = ["uint m = " ++ intercalate " * " (map (\f -> f ++ "_m") fs) ++ ";"] in
    let (colMapData, s0, s2) = processTables 0 [] fs zs in

    let colMap = map (\(x,j) -> (x,[colPrefix ++ show j])) colMapData in
    (M.fromListWith (++) colMap, s0 ++ s1 ++ s2)

-- TODO some indices still are mismatching here
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
-- TODO point to particular argument name in argMap, not to an index, as they may have different types
aexprToSecreC :: S.Set VName -> (M.Map Arg Int) -> (M.Map Arg [String]) -> Int -> AExpr Var -> (Int,String,String,String,S.Set VName,[String])
aexprToSecreC declaredVars argMap varMap c' aexpr =
    let b = nv0 ++ show c' in
    let c = c' + 1 in
    case aexpr of

        AVar z ->
            case z of
                  Private VarNum x  -> (c',x,"D", "int32", declaredVars,[])
                  Private VarText x -> (c',x,"D","uint32", declaredVars,[])
                  Public  VarNum x  -> (c',x,"",  "int32", declaredVars,[])
                  Public  VarText x -> (c',x,"", "uint32", declaredVars,[])
                  Free x            -> (c',"args[" ++ show (argMap ! aexpr) ++ "]", "D", "T", declaredVars,[])

        AConstBool x -> (c, case x of {True -> "true"; False -> "false"},"","bool",  declaredVars,[])
        AConstNum x  -> (c, show x,                                      "","int32", declaredVars,[])
        AConstStr x  -> (c, "CRC32(" ++ show x ++ ")",                   "","uint32",declaredVars,[])

        ANary (AMember pred) as -> let c'' = c + length as in
                                   let bs = map (\z -> nv0 ++ show z) [c..c''-1] in
                                   let (dt,dv,s2) = foldl (\(t0,dv0,ys0) (a,b,i) ->
                                                               let (t1,dv1,ys1) = processArg pred dv0 a b i in
                                                               (join t0 t1,dv1,ys0 ++ ys1)
                                                          ) ("",declaredVars, []) $ zip3 as bs [0..] in

                                   let s3 = ["D bool [[1]] " ++ b ++ " = (" ++ intercalate " & " bs ++ ");"] in
                                   (c'',b, dt,"bool",dv, s2 ++ s3)

        ANary AAnds xs -> processRecNary "&" c b xs
        ANary AOrs  xs -> processRecNary "|" c b xs
        ANary ASum xs  -> processRecNary "+" c b xs
        ANary AProd xs -> processRecNary "*" c b xs

        AUnary ANot x -> processRecUnary "!" c b x
        AUnary ANeg x -> processRecUnary "-" c b x

        ABinary ADiv x1 x2  -> processRecBinary "" "/" c b x1 x2
        ABinary AMult x1 x2 -> processRecBinary "" "*" c b x1 x2
        ABinary AAdd x1 x2  -> processRecBinary "" "+" c b x1 x2
        ABinary ASub x1 x2  -> processRecBinary "" "-" c b x1 x2
        ABinary AMin x1 x2  -> processRecBinary "" "min" c b x1 x2
        ABinary AMax x1 x2  -> processRecBinary "" "max" c b x1 x2

        ABinary AAnd x1 x2 -> processRecBinary "" "&" c b x1 x2
        ABinary AOr  x1 x2 -> processRecBinary "" "|" c b x1 x2
        ABinary ALT x1 x2  -> processRecBinary "bool" "<" c b x1 x2
        ABinary ALE x1 x2  -> processRecBinary "bool" "<=" c b x1 x2
        ABinary AEQ x1 x2  -> processRecBinary "bool" "==" c b x1 x2
        ABinary AGE x1 x2  -> processRecBinary "bool" ">=" c b x1 x2
        ABinary AGT x1 x2  -> processRecBinary "bool" ">" c b x1 x2

    where
          processRecUnary op c b x      = let (c'',b', ptype, dtype, declaredVars', ys) = aexprToSecreC declaredVars argMap varMap c x in
                                              (c'',b,  ptype, dtype, declaredVars', ys ++ [ptype ++ " " ++ dtype ++ " [[1]] " ++ b ++ " = " ++ op ++ "(" ++ b' ++ ");"])

          processRecBinary dtype0 op c b x1 x2 = let (c1,b1,ptype1,dtype1,dv1,ys1) = aexprToSecreC declaredVars argMap varMap c x1 in
                                          let (c2,b2,ptype2,dtype2,dv2,ys2) = aexprToSecreC dv1 argMap varMap c1 x2 in
                                          let ptype = join ptype1 ptype2 in
                                          let dtype = if dtype0 /= "" then dtype0 else join2 op dtype1 dtype2 in
                                          (c2, b, ptype, dtype, dv2, ys1 ++ ys2 ++ [ptype ++ " " ++ dtype ++ " [[1]] " ++ b ++ " = " ++ b1 ++ " " ++ op ++ " " ++ b2 ++ ";"])

          processRecBinaryPrefix op c b x1 x2 = let (c1,b1,ptype1,dtype1,dv1,ys1) = aexprToSecreC declaredVars argMap varMap c x1 in
                                                let (c2,b2,ptype2,dtype2,dv2,ys2) = aexprToSecreC dv1 argMap varMap c1 x2 in
                                                let ptype = join ptype1 ptype2 in
                                                let dtype = if (dtype1 == dtype2) then dtype1 else error $ error_typeOp op dtype1 dtype2 in
                                                (c2, b, ptype, dtype, dv2, ys1 ++ ys2 ++ [ptype ++ " " ++ dtype ++ " [[1]] " ++ b ++ " = " ++ op ++ "(" ++ b1 ++ "," ++ b2 ++ ");"])

          processRecNary op c b xs  = let (c'',bs, ptype, dtype, dv, ys) = foldl (\(c0, bs0, ptype0, dtype0, dv0, ys0) x ->
                                                                                 let (c1, b1, ptype1, dtype1, dv1, ys1) = aexprToSecreC dv0 argMap varMap c0 x in
                                                                                 let ptype = join ptype1 ptype0 in
                                                                                 let dtype = if dtype0 == "" || (dtype1 == dtype0) then dtype1
                                                                                             else error $ error_typeOp op dtype1 dtype0 in
                                                                                 (c1, bs0 ++ [b1], ptype, dtype, dv1, ys0 ++ ys1)
                                                                             ) (c,[],"","",S.empty,[]) xs in

                                      (c'',b, ptype, dtype, dv, ys ++ [ptype ++ " " ++ dtype ++ " [[1]] " ++ b ++ " = (" ++ intercalate op bs ++ ");"])

          -- TODO we need to take into account _all_ matchings of varMap, not only the head
          processArg pred dv arg b' i =
              case arg of

                      AVar (Private VarNum x)  -> let (dv', decl) = if S.member x dv then
                                                                        (dv, "temp_D_bool = (temp_D_int32 == " ++ x ++ ");")
                                                                    else
                                                                        (S.insert x dv, "D int32 [[1]] " ++ x ++ " = temp_D_int32; temp_D_bool = reshape(true,m);")
                                                  in ("D",dv', ["temp_D_int32 = " ++ head (varMap ! arg) ++ ";", decl,
                                                               "D bool [[1]] " ++ b' ++ " = temp_D_bool & " ++
                                                               if M.member arg argMap then "(args[" ++ show (argMap ! arg) ++ "] == " ++ x ++ ");"
                                                               else "true;"])

                      AVar (Private VarText x) -> let (dv', decl) = if S.member x dv then
                                                                        (dv, "temp_D_bool = (temp_D_xor_uint32 == " ++ x ++ ");")
                                                                    else
                                                                        (S.insert x dv, "D xor_uint32 [[1]] " ++ x ++ " = temp_D_xor_uint32; temp_D_bool = reshape(true,m);")

                                                  in("D",dv', ["temp_D_xor_uint32 = " ++ head (varMap ! arg) ++ ";", decl,
                                                              "D bool [[1]] " ++ b' ++ " = temp_D_bool & " ++
                                                              if M.member arg argMap then "(args[" ++ show (argMap ! arg) ++ "] == " ++ x ++ ");"
                                                              else "true;"])

                      -- TODO we do not have public column in tdbhdf5, think whether we need to support this at all
                      AVar (Public  VarNum x)  -> let (dv', decl) = if S.member x dv then
                                                                        (dv, "temp_bool = (temp_int32 == " ++ x ++ ");")
                                                                    else
                                                                        (S.insert x dv, "int32 [[1]] " ++ x ++ " = temp_int32; temp_bool = reshape(true,m);")
                                                  in ("",dv', ["temp_int32 = " ++ head (varMap ! arg) ++ ";", decl,
                                                              "D bool [[1]] " ++ b' ++ " = temp_bool & " ++
                                                              if M.member arg argMap then "(args[" ++ show (argMap ! arg) ++ "] == " ++ x ++ ");"
                                                              else "true;"])

                      AVar (Public  VarText x) -> let (dv', decl) = if S.member x dv then
                                                                        (dv, "temp_bool = (temp_uint32 == " ++ x ++ ");")
                                                                    else
                                                                        (S.insert x dv, "uint32 [[1]] " ++ x ++ " = temp_uint32; temp_bool = reshape(true,m);")

                                                   in("",dv', ["temp_uint32 = " ++ head (varMap ! arg) ++ ";", decl,
                                                              "D bool [[1]] " ++ b' ++ " = temp_bool & " ++
                                                              if M.member arg argMap then "(args[" ++ show (argMap ! arg) ++ "] == " ++ x ++ ");"
                                                              else "true;"])

                      AVar (Free z)  -> let x = if M.member arg argMap then "args[" ++ show (argMap ! arg) ++ "]"
                                                else error $ error_nonGroundTableVar pred z i
                                        in ("D",dv,["D bool [[1]] " ++ b' ++ " = (" ++ x ++ " == tdbReadColumn(ds," ++ show pred ++ ", " ++ show i ++ " :: uint));"])

                      AConstNum z  -> ("D",dv,["D bool [[1]] " ++ b' ++ " = (" ++ show z ++ " == tdbReadColumn(ds," ++ show pred ++ ", " ++ show i ++ " :: uint));"])
                      AConstStr z  -> ("D",dv,["D bool [[1]] " ++ b' ++ " = (CRC32(" ++ show z ++ ") == tdbReadColumn(ds," ++ show pred ++ ", " ++ show i ++ " :: uint));"])

                      _                        -> error $ error_complexExpression arg



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


