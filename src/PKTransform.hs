{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module PKTransform (pkTransform) where

import Control.Lens
import Control.Monad.State.Strict

import qualified Data.Map.Strict as M
import Data.Maybe

import Annotation
import DatalogProgram
import Expr
import Expr.Classes
import Rule

type PKTrans = State PKState

data PKState = PKState
  { _pksVarIdx  :: !Int
  , _pksPKMap   :: !(M.Map (String, String) [Expr])
  , _pksProgram :: !DatalogProgram
  }
makeLenses ''PKState

instance HasVarIdx PKState where
  idxLens = pksVarIdx

pkState :: DatalogProgram -> PKState
pkState dp = PKState 0 M.empty dp

-- Reduces database lookups
pkTransform :: DatalogProgram -> DatalogProgram
pkTransform dp = dp & dpRules . traversed . ruleTail %~ foldWithAnds . f
  where f e = evalState (traverse transformDBFact $ andsToList e) (pkState dp)

transformDBFact :: Expr -> PKTrans Expr
transformDBFact p@Pred{} =
  do
    dp <- use pksProgram
    pkMap <- use pksPKMap
    let predId = p ^. predName
        pArgs = p ^. predArgs
    fromMaybe (return p) $
      do
        dbFact <- findDBFact dp predId
        pkIdx <- dbFact ^. pkIndex
        pkId <- identifier $ pArgs !! pkIdx
        let vars = M.lookup (predId, pkId) pkMap
            zipper x y =
              do
                guard $ x ^. annotation . isPK . to not
                let x' = x & annotation . annBound .~ True
                return $ eUn x' y
            equalities x = catMaybes $ zipWith zipper x pArgs
            updateMap = modifying pksPKMap $ M.insert (predId, pkId) pArgs
        Just $ case vars of
          Just x  -> return . foldWithAnds $ equalities x
          Nothing -> updateMap *> return p
transformDBFact x = return x

