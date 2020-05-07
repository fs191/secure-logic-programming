{-# LANGUAGE DeriveFunctor #-}

module Table 
  ( Table, Row, Column
  , table
  , header
  , rows
  , columns
  , lookupColumn
  , lookupValue
  , values
  ) where

import Data.Map as M
import Data.List (transpose, find)

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
  | length ks == length t = Table ks t
  | otherwise = error "header and table lengths do not match"

header :: Table k v -> [k]
header = _tHeader

rows :: (Ord k) => Table k v -> [Row k v]
rows t = Row . fromList . (zip $ header t) <$> _tRows t

columns :: Table k v -> [Column k v]
columns t = uncurry Column <$> zip (header t) (transpose $ _tRows t)

lookupColumn :: (Eq k) => Table k v -> k -> Maybe (Column k v)
lookupColumn t k = find (\x -> k == _cKey x) $ columns t

lookupValue :: (Ord k) => Row k v -> k -> Maybe v
lookupValue (Row m) k = M.lookup k m

values :: Row k v -> [v]
values (Row m) = elems m


