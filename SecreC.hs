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
nv0 = "_b"

createHeader = [
    -- import essentials
    "import stdlib;",
    "import shared3p;",
    "import shared3p_string;",
    "import shared3p_table_database;",
    "import table_database;\n",
    "domain pd_shared3p shared3p;\n"]

-- a SecreC program is a list of code lines
-- if no particular goal is given, we just 
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
    if not(isGround bexpr) then [] else
    let declaration = [predToString "//" pname as bexpr,
                       "template <domain D, type T>",
                       "D bool _goal_" ++ pname ++ "(D T [[1]] _args){",
                       indent ++ "uint _m;",
                       indent ++ "uint _rv;",
                       indent ++ "string _ds = \"DS1\";"] in
    let argMap = M.fromList $ zip as [0..] in
    let (_,b,body) = aexprToSecreC argMap 0 (foldBool bexpr) in
    let footer = [indent ++ "tdbCloseConnection(ds);",
                  indent ++ "return all(" ++ b ++ ");",
                  "}\n\n"] in
    declaration ++ (map (indent ++) body) ++ footer

-- this assumes that the expression is "folded", i.e. is in DNF form with grouped AND / OR
-- TODO this does not work with reused variables in several database predicates, need a different variable handling
aexprToSecreC :: (M.Map Arg Int) -> Int -> AExpr Var -> (Int,String,[String])
aexprToSecreC argMap c' aexpr =

    let b = nv0 ++ show c' in
    let c = c' + 1 in
    case aexpr of

        AVar z ->
            case z of
                  Private VarNum x  -> (c',x,[])
                  Private VarText x -> (c',x,[])
                  Public  VarNum x  -> (c',x,[])
                  Public  VarText x -> (c',x,[])

        -- TODO process these similarly to AVar
        AConstBool x -> (c,b,["D bool [[1]] "       ++ b ++ " = " ++ case x of {True -> "true"; False -> "false"} ++ ";"])
        AConstNum x  -> (c,b,["D int32 [[1]] "     ++ b ++ " = " ++ show x ++ ";"])
        AConstStr x  -> (c,b,["D xor_uint32 [[1]] " ++ b ++ " = CRC32(" ++ show x ++ ");"])

        ANary (AMember pred) as -> let s1 = ["_m = tdbGetRowCount(_ds," ++ show pred ++ ");"] in
                                   let c'' = c + length as in
                                   let bs = map (\z -> nv0 ++ show z) [c..c''-1] in
                                   let s2 = concat $ zipWith3 (processArg pred) as bs [0..] in
                                   let s3 = ["D bool [[1]] " ++ b ++ " = (" ++ intercalate " & " bs ++ ");"] in
                                   (c'',b,s1 ++ s2 ++ s3)

        ANary AAnds xs -> processRecNary "&" c b xs
        ANary AOrs  xs -> processRecNary "|" c b xs
        ANary ASum xs  -> processRecNary "+" c b xs
        ANary AProd xs -> processRecNary "*" c b xs

        AUnary ANeg x -> processRecUnary "-" c b x
        AUnary ANot x -> processRecUnary "!" c b x

        ABinary ADiv x1 x2  -> processRecBinary "/" c b x1 x2
        ABinary AMult x1 x2 -> processRecBinary "*" c b x1 x2
        ABinary AAdd x1 x2 -> processRecBinary "+" c b x1 x2
        ABinary ASub x1 x2 -> processRecBinary "-" c b x1 x2
        ABinary AMin x1 x2 -> processRecBinaryPrefix "min" c b x1 x2
        ABinary AMax x1 x2 -> processRecBinaryPrefix "max" c b x1 x2
        ABinary AAnd x1 x2 -> processRecBinary "&" c b x1 x2
        ABinary AOr  x1 x2 -> processRecBinary "|" c b x1 x2
        ABinary ALT x1 x2  -> processRecBinary "<" c b x1 x2
        ABinary ALE x1 x2  -> processRecBinary "<=" c b x1 x2
        ABinary AEQ x1 x2  -> processRecBinary "==" c b x1 x2
        ABinary AGE x1 x2  -> processRecBinary ">=" c b x1 x2
        ABinary AGT x1 x2  -> processRecBinary ">" c b x1 x2

    where
          processRecUnary op c b x      = let (c'',b', ys) = aexprToSecreC argMap c x in
                                          (c'',b, ys ++ ["D bool [[1]] " ++ b ++ " = " ++ op ++ "(" ++ b' ++ ");"])

          processRecBinary op c b x1 x2 = let (c1,b1,ys1) = aexprToSecreC argMap c  x1 in
                                          let (c2,b2,ys2) = aexprToSecreC argMap c1 x2 in
                                          (c2, b, ys1 ++ ys2 ++ ["D bool [[1]] " ++ b ++ " = " ++ b1 ++ " " ++ op ++ " " ++ b2 ++ ";"])

          processRecBinaryPrefix op c b x1 x2 = let (c1,b1,ys1) = aexprToSecreC argMap c  x1 in
                                                let (c2,b2,ys2) = aexprToSecreC argMap c1 x2 in
                                                (c2, b, ys1 ++ ys2 ++ ["D bool [[1]] " ++ b ++ " = " ++ op ++ "(" ++ b1 ++ "," ++ b2 ++ ");"])

          processRecNary op c b xs  = let (c'',bs, ys) = foldl (\(c0, bs0, ys0) x -> let (c1, b1, ys1) = aexprToSecreC argMap c0 x in (c1, bs0 ++ [b1], ys0 ++ ys1)) (c,[],[]) xs in
                                      (c'',b, ys ++ ["D bool [[1]] " ++ b ++ " = (" ++ intercalate op bs ++ ");"])

          processArg pred arg b' i =
              case arg of

                  AVar (Private VarNum x)  -> ["D int32 [[1]] " ++ x ++ " = tdbReadColumn(_ds," ++ show pred ++ ", " ++ show i ++ " :: uint);",
                                               "bool " ++ b' ++ " = true;"]
                  AVar (Private VarText x) -> ["D xor_uint32 [[1]] " ++ x ++ "(_m);",
                                               "_rv = tdbReadColumn(_ds," ++ show pred ++ ", " ++ show i ++ " :: uint);",
                                               "for (uint _i = 0; _i < _m; _i++){",
                                               indent ++ "D xor_uint8 [[1]] _temp = tdbVmapGetVlenValue(_rv, \"values\", _i);",
                                               indent ++ x ++ "[_i] = CRC32(_temp);",
                                               "}",
                                               "D bool [[1]] " ++ b' ++ " = " ++ if M.member arg argMap then "(_args[" ++ show (argMap ! arg) ++ "] == " ++ x ++ ");" else "true;"]

                  AVar (Public  VarNum x)  -> ["int32 [[1]] " ++ x ++ " = tdbReadColumn(_ds," ++ show pred ++ ", " ++ show i ++ " :: uint);",
                                               "bool " ++ b' ++ " = true;"]
                  AVar (Public  VarText x) -> ["uint32 [[1]] " ++ x ++ "(_m);",
                                               "_rv = tdbReadColumn(_ds," ++ show pred ++ ", " ++ show i ++ " :: uint);",
                                               "for (uint _i = 0; _i < _m; _i++){",
                                               indent ++ "D xor_uint8 [[1]] _temp0 = tdbVmapGetVlenValue(_rv, \"values\", _i);",
                                               indent ++ "uint32 _temp = declassify(CRC32(_temp0));",
                                               indent ++ x ++ "[_i] = _temp;",
                                               "}",
                                               "D bool [[1]] " ++ b' ++ " = " ++ if M.member arg argMap then "(_args[" ++ show (argMap ! arg) ++ "] == " ++ x ++ ");" else "true;"]

                  -- TODO add handling of constants as well
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


