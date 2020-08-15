{-# LANGUAGE DeriveFunctor #-}

module Table 
  ( getTableData
  ) where

import Data.Map as M
import Data.List (transpose, find)
import Data.List.Split (endBy, splitOn)

newtype Row k v = Row (Map k v)
  deriving (Functor)
data Column k v = Column 
  { _cKey    :: k 
  , _cValues :: [v]
  }
  deriving (Functor)

data Table k v = Table
  { _tHeader :: [k]
  , _tRows   :: [[v]]
  }

table :: [k] -> [[v]] -> Table k v
table ks t
  | length ks == length (transpose t) = Table ks t
  | otherwise = error "header and table lengths do not match"

header :: Table k v -> [k]
header = _tHeader

rows :: (Ord k) => Table k v -> [Row k v]
rows t = Row . fromList . (zip $ header t) <$> _tRows t

rowsRaw :: Table k v -> [[v]]
rowsRaw (Table _ vss) = vss

columns :: Table k v -> [Column k v]
columns t = uncurry Column <$> zip (header t) (transpose $ _tRows t)

lookupColumn :: (Eq k) => Table k v -> k -> Maybe (Column k v)
lookupColumn t k = find (\x -> k == _cKey x) $ columns t

lookupValue :: (Ord k) => Row k v -> k -> Maybe v
lookupValue (Row m) k = M.lookup k m

values :: Row k v -> [v]
values (Row m) = elems m


dbPath = "examples/database/"
dbSep  = ";"
dbExt  = ".csv"

-- read the database from the file as a matrix of strings
-- read is as a single table row
readDBString :: String -> String -> IO (Table String String)
readDBString dbFileName separator = do
    (firstLine:ls) <- fmap lines (readFile dbFileName)
    let varNames = splitBySeparator separator firstLine
    let t    = Prelude.map (splitBySeparator separator) ls
    return $ table varNames t

splitBySeparator :: String -> String -> [String]
splitBySeparator sep s =
   if last s == last sep then endBy sep s else splitOn sep s

getTableData :: String -> IO [[String]]
getTableData p = do
    tableData <- readDBString (dbPath ++ p ++ dbExt) dbSep
    let tableHeader = header tableData
    let tableRows   = rowsRaw tableData
    return (tableHeader : tableRows)

