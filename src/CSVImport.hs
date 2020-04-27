module CSVImport
  ( generateDataToDBscript
  ) where

---------------------------------------------------------
---- Uploads table data to Sharemind servers
----  as secred-shared values
---- this naive approach hard-codes data into SecreC file
---- WARNING: do not use it with actual private data!!!!
---------------------------------------------------------

import Data.List
import Data.Bits
import Data.Char
import Data.List.Split
import qualified Data.Map as M
import Debug.Trace
import DatalogProgram

import Aexpr
import Rule
import ErrorMsg

indent = "    "

type PMap = M.Map [Term] Formula

splitBySeparator :: String -> String -> [String]
splitBySeparator sep s =
   if last s == last sep then endBy sep s else splitOn sep s

-- read the DB line by line -- no speacial parsing, assume that the delimiters are whitespaces
readInput :: String -> IO String
readInput path = do
   content <- readFile path
   return content

-- read the database from the file as a matrix of strings
-- read is as a single table row
readDBString :: String -> String -> IO ([String], [[String]])
readDBString dbFileName separator = do
    (firstLine:ls) <- fmap lines (readInput dbFileName)
    let varNames = splitBySeparator separator firstLine
    let table    = map (splitBySeparator separator) ls
    return (varNames, table)

createHeader = [
    -- import essentials
    "import stdlib;",
    "import shared3p;",
    "import shared3p_table_database;",
    "import table_database;\n",
    "domain pd_shared3p shared3p;\n",
    "void main(){",
    indent ++ "string _tableName;",
    indent ++ "string _ds = \"DS1\";",
    indent ++ "uint _params;",

    -- TODO we do not have public columns in tdbhf5, so public becomes secret-shared as well
    indent ++ "pd_shared3p int32 _temp_int32;",
    indent ++ "pd_shared3p uint32 _temp_uint32;",
    indent ++ "pd_shared3p bool _temp_bool;",
    indent ++ "pd_shared3p xor_uint8 _temp_string;",

    indent ++ "pd_shared3p int32 _temp_D_int32;",
    indent ++ "pd_shared3p xor_uint32 _temp_D_xor_uint32;",
    indent ++ "pd_shared3p bool _temp_D_bool;",
    indent ++ "pd_shared3p xor_uint8 _temp_D_string;"]

generateDataToDBscript :: PPDatalogProgram -> IO (String)
generateDataToDBscript database = do
    dataMap <- extractDataFromTables undefined
    --return $ intercalate "\n" $ createCSVImport dataMap
    return undefined

extractDataFromTables :: M.Map PName PMap -> IO (M.Map PName (M.Map AName DBVar, [AName], [[String]]))
extractDataFromTables database = do
    -- mapM is a standard function [IO a] -> IO [a]
    res <- mapM extractDataFromTable (M.toList database)
    return $ M.fromList res

-- TODO generalize the separator
extractDataFromTable :: (PName,PMap) -> IO (PName, (M.Map AName DBVar, [AName], [[String]]))
extractDataFromTable (pname,argMap) = do
    (xs,yss) <- readDBString ("examples/database/" ++ pname ++ ".csv") ";"
    let typeMap = M.fromList $ map processArg $ head (M.keys argMap)
    return $ (pname, (typeMap,xs,yss))
    where processArg arg =
              case arg of
                  AVar v@(Bound _ _ z) -> (z,v)

-- a SecreC program is a list of code lines
-- if no particular goal is given, we just
createCSVImport :: M.Map PName (M.Map AName DBVar, [AName], [[String]]) -> [String]
createCSVImport dataMap =
    let header = createHeader in
    let body   = map (indent ++ ) $ concat $ (M.elems (M.mapWithKey createTable dataMap)) in
    let footer = [indent ++ "tdbCloseConnection(_ds);", "}\n"] in
    header ++ body ++ footer

createTable :: PName -> (M.Map AName DBVar, [AName], [[String]]) -> [String]
createTable pname (typeMap, xs, yss) =

    let header1 = ["_tableName  = \"" ++ pname ++ "\";",
                  "tdbOpenConnection(_ds);",
                  "if (tdbTableExists(_ds, _tableName)) {",
                  "    print(\"Deleting existing table: \" + _tableName);",
                  "    tdbTableDelete(_ds, _tableName);",
                  "}",
                  "print(\"Start creating the new table: \" + _tableName);",
                  "_params = tdbVmapNew();"]

    in let header2 = concat $ map (createTableHeader pname typeMap) xs
    in let header3 = ["tdbTableCreate(_ds, _tableName, _params);",
                      "tdbVmapDelete(_params);",
                      "_params = tdbVmapNew();"]

    in header1 ++ header2 ++ header3 ++ createTableRec pname typeMap xs yss

createTableHeader :: PName -> M.Map AName DBVar -> AName -> [String]
createTableHeader pname typeMap x =
    let dtype = (if M.member x typeMap then typeMap ! x else error $ error_unknownColumn x pname) in
    let (isVlen,dvar,var) = case dtype of
                Bound Public  VarNum  z -> ("","_temp_int32",z)
                Bound Private VarNum  z -> ("","_temp_D_int32",z)

                Bound Public  VarText z -> ("Vlen","_temp_string",z)
                Bound Private VarText z -> ("Vlen","_temp_D_string",z)
                _                       ->  error $ error_unsupportedColumnType x

    in ["tdbVmapAdd" ++ isVlen ++ "Type(_params, \"types\", " ++ dvar ++ ");",
        "tdbVmapAddString(_params, \"names\", \"" ++ var ++ "\");"]


createTableRec :: PName -> M.Map AName DBVar -> [AName] -> [[String]] -> [String]
createTableRec _ _ _ [] = []
createTableRec pname typeMap xs (ys:yss) =
    let s0 = ["tdbInsertRow(_ds," ++ show pname ++ ", _params);", "tdbVmapClear(_params);\n"] in
    let s = createRow pname typeMap xs ys in
    s ++ s0 ++ createTableRec pname typeMap xs yss

createRow :: PName -> M.Map AName DBVar -> [AName] -> [String] -> [String]
createRow _ _ [] [] = []
createRow pname _ [] ys  = error $ error_dbFileLengthsTooMany pname ys
createRow pname _ xs  [] = error $ error_dbFileLengthsTooFew pname xs
createRow pname typeMap (x:xs) (y:ys) =
    let dtype = (if M.member x typeMap then typeMap ! x else error $ error_unknownColumn x pname) in
    -- TODO we do not have public columns in tdbhf5, so public becomes secret-shared as well
    let s = case dtype of
                Bound Public  VarNum  z -> ["{pd_shared3p int32 " ++ z ++ " = " ++ y ++ ";",
                                            " tdbVmapAddValue(_params, \"values\", " ++ z ++ ");}"]
                Bound Private VarNum  z -> ["{pd_shared3p int32 " ++ z ++ " = " ++ y ++ ";",
                                            " tdbVmapAddValue(_params, \"values\", " ++ z ++ ");}"]

                Bound Public  VarText z -> ["{pd_shared3p xor_uint8 [[1]] " ++ z ++ " = __bytes_from_string(\"" ++ y ++ "\");",
                                            " tdbVmapAddVlenValue(_params, \"values\", " ++ z ++ ");}"]
                Bound Private VarText z -> ["{pd_shared3p xor_uint8 [[1]] " ++ z ++ " = __bytes_from_string(\"" ++ y ++ "\");",
                                            " tdbVmapAddVlenValue(_params, \"values\", " ++ z ++ ");}"]
                _                       ->  error $ error_unsupportedColumnType x

    in s ++ createRow pname typeMap xs ys


