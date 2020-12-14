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

trueColumn :: [SCExpr] -> SCExpr
trueColumn  = SCFunCall "trueColumn"

colSize :: [SCExpr] -> SCExpr
colSize  = SCFunCall "colSize"

size :: [SCExpr] -> SCExpr
size     = SCFunCall "size"

sum :: [SCExpr] -> SCExpr
sum      = SCFunCall "sum"

argument :: [SCExpr] -> SCExpr
argument   = SCFunCall "argument"

lpShuffle :: [SCExpr] -> SCExpr
lpShuffle  = SCFunCall "lpShuffle"

aop :: SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr
aop op x y b = SCFunCall "aop" [op, x, y, b]

bop :: SCExpr -> SCExpr -> SCExpr -> SCExpr -> SCExpr
bop op x y b = SCFunCall "bop" [op, x, y, b]

findRepeating :: [SCExpr] -> SCExpr
findRepeating  = SCFunCall "findRepeating"

copyColumn :: SCExpr -> SCExpr
copyColumn x = SCFunCall "copyColumn" [x]

countSortPermutation :: [SCExpr] -> SCExpr
countSortPermutation = SCFunCall "countSortPermutation"

quickSortPermutation :: [SCExpr] -> SCExpr
quickSortPermutation = SCFunCall "quickSortPermutation"

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

filterTrue :: [SCExpr] -> SCExpr
filterTrue     = SCFunCall "filterTrue"

declassifyIfNeed :: [SCExpr] -> SCExpr
declassifyIfNeed     = SCFunCall "declassifyIfNeed"

tdbGetRowCount :: [SCExpr] -> SCExpr
tdbGetRowCount = SCFunCall "tdbGetRowCount"

publishColumn :: [SCExpr] -> Statement
publishColumn         = FunCall "publishCol"

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

