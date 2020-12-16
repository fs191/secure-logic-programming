module Language.SecreC.Builtin where

import Relude

import Language.SecreC.SCExpr
import Language.SecreC.SCProgram

-- some functions that are already defined in SecreC
extendColumn :: SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr
extendColumn x m mi ni = SCFunCall "extendColumn" [x, m, mi, ni]

getDBColumn :: SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr
getDBColumn ds tableName colIndex m mi ni = SCFunCall "getDBColumn" [ds, tableName, colIndex, m, mi, ni]

myCat :: SCExpr -> SCExpr -> SCExpr
myCat x y = SCFunCall "myCat" [x, y]

applyPermutation :: SCExpr -> SCExpr -> SCExpr
applyPermutation a p = SCFunCall "applyPermutation" [a, p]

reshape :: SCExpr -> [SCExpr] -> SCExpr
reshape x xs = SCFunCall "reshape" $ x:xs

constIntColumn :: SCExpr -> SCExpr -> SCExpr
constIntColumn arg0 m = SCFunCall "constIntColumn" [arg0, m]

constFloatColumn :: SCExpr -> SCExpr -> SCExpr
constFloatColumn arg0 m = SCFunCall "constFloatColumn" [arg0, m]

constBoolColumn :: SCExpr -> SCExpr -> SCExpr
constBoolColumn arg0 m = SCFunCall "constBoolColumn" [arg0, m]

constStrColumn :: SCExpr -> SCExpr -> SCExpr
constStrColumn arg0 m = SCFunCall "constStrColumn" [arg0, m]

constColumn :: SCExpr -> SCExpr
constColumn arg0 = SCFunCall "constColumn" [arg0]

trueColumn :: SCExpr -> SCExpr
trueColumn m = SCFunCall "trueColumn" [m]

colSize :: SCExpr -> SCExpr
colSize x = SCFunCall "colSize" [x]

size :: SCExpr -> SCExpr
size x = SCFunCall "size" [x]

sum :: SCExpr -> SCExpr
sum x = SCFunCall "sum" [x]

argument :: SCExpr -> SCExpr
argument x = SCFunCall "argument" [x]

lpShuffle :: SCExpr -> SCExpr
lpShuffle x = SCFunCall "lpShuffle" [x]

aop :: SCExpr -> SCExpr -> SCExpr -> SCExpr
aop op x y = SCFunCall "aop" [op, x, y]

bop :: SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr
bop op x y b = SCFunCall "bop" [op, x, y, b]

findRepeating :: SCExpr -> SCExpr
findRepeating x = SCFunCall "findRepeating" [x]

copyColumn :: SCExpr -> SCExpr
copyColumn x = SCFunCall "copyColumn" [x]

countSortPermutation :: SCExpr -> SCExpr
countSortPermutation x = SCFunCall "countSortPermutation" [x]

quickSortPermutation :: SCExpr -> SCExpr
quickSortPermutation x = SCFunCall "quickSortPermutation" [x]

countFilter :: SCExpr -> SCExpr ->SCExpr -> SCExpr
countFilter x b m = SCFunCall "count_filter" [x, b, m]

sumFilter :: SCExpr -> SCExpr ->SCExpr -> SCExpr
sumFilter x b m = SCFunCall "sum_filter" [x, b, m]

avgFilter :: SCExpr -> SCExpr ->SCExpr -> SCExpr
avgFilter x b m = SCFunCall "avg_filter" [x, b, m]

minFilter :: SCExpr -> SCExpr ->SCExpr -> SCExpr
minFilter x b m = SCFunCall "min_filter" [x, b, m]

maxFilter :: SCExpr -> SCExpr ->SCExpr -> SCExpr
maxFilter x b m = SCFunCall "max_filter" [x, b, m]

timesFilter :: SCExpr -> SCExpr ->SCExpr -> SCExpr
timesFilter x b m = SCFunCall "times_filter" [x, b, m]

filterTrue :: SCExpr -> SCExpr -> SCExpr -> SCExpr
filterTrue x y z = SCFunCall "filterTrue" [x, y, z]

declassifyIfNeed :: SCExpr -> SCExpr
declassifyIfNeed x = SCFunCall "declassifyIfNeed" [x]

tdbGetRowCount :: SCExpr -> SCExpr -> SCExpr
tdbGetRowCount ds tk = SCFunCall "tdbGetRowCount" [ds, tk]

publishColumn :: SCExpr -> SCExpr -> SCExpr -> Statement
publishColumn x y z = FunCall "publishCol" [x, y, z]

createTable :: SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr -> Statement
createTable ds tableName colDataType colDomain headers ns = FunCall "createTable"
  [ds, tableName, colDataType, colDomain, headers, ns]

writePublicToTable :: SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr -> Statement
writePublicToTable ds tableName colDataType colDomain boolData intData floatData strData ms = FunCall "writePublicToTable" 
  [ds, tableName, colDataType, colDomain, boolData, intData, floatData, strData, ms]

tdbOpenConnection :: SCExpr -> Statement
tdbOpenConnection datasource = FunCall "tdbOpenConnection" [datasource]

tdbCloseConnection :: SCExpr -> Statement
tdbCloseConnection datasource = FunCall "tdbCloseConnection" [datasource]

