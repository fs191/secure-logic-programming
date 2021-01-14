{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils.TableGen 
  ( csvImportCode
  , CsvImportOptions(..)
  ) where

import Relude
import qualified Data.Text as T
import Control.Lens

import DatalogProgram as DP
import Language.SecreC.SCProgram as P
import Language.SecreC.SCExpr
import Language.SecreC.Builtin as BI
import Expr
import Utils.Table

data CsvImportOptions = CsvImportOptions
  { _poInFilePath :: Text
  , _poOutFilePath :: Text
  , _poDataPath :: Text
  }
makeLenses ''CsvImportOptions

csvImportCode 
  :: ( MonadIO m
     , MonadFail m
     , MonadReader CsvImportOptions m
     )
  => DP.DatalogProgram 
  -> m SCProgram
csvImportCode dp = do
  let ds = "ds"
  let extPreds = dp ^.. DP.dpDBClauses
  dataPath <- view poDataPath
  tableData <- forM extPreds $ \p -> do
    let name = view predName p
    getTableData $ dataPath <> "/" <> name <> ".csv"
  return $ SCProgram $ hdr
                     <> [Funct . mainFun $
                                     [ VarInit (SCVar SCPublic SCText ds) (SCConstStr "DS1")
                                     , BI.tdbOpenConnection $ SCVarName ds]
                                     <> concat (zipWith (tableGenerationCode ds) extPreds tableData) <>
                                     [BI.tdbCloseConnection $ SCVarName ds]]

tableGenerationCode :: Text -> Expr -> [[Text]] -> [Statement]
tableGenerationCode ds dbc (tableHeader:tableRows) =

  [ BI.createTable  (SCVarName ds) 
                    (SCConstStr p) 
                    (SCConstArr (map SCConstInt types)) 
                    (SCConstArr (map SCConstInt domains))
                    (SCConstStr headerStr) 
                    (SCConstArr (map SCConstInt hlengths))
  , BI.writePublicToTable (SCVarName ds) 
                          (SCConstStr p) 
                          (SCConstArr (map SCConstInt types)) 
                          (SCConstArr (map SCConstInt domains))
                          (SCConstArr (map SCConstAny boolData))
                          (SCConstArr (map SCConstAny intData))
                          (SCConstArr (map SCConstAny floatData))
                          (SCConstStr strData)
                          (SCConstArr (map SCConstInt vlengths))
  ]

  where
        p  = dbc ^. predName
        xs = dbc ^. predArgs
        types = map (\x -> let dtype  = x ^. annotation . annType in
                        case dtype of
                            PPBool  -> 0
                            PPInt64   -> 1
                            PPFloat32 -> 2
                            PPStr   -> 3
                            _       -> error "Can only create a table for bool, int, float, string datatypes."
                ) xs

        domains = map (\x -> let dom  = x ^. annotation . domain in
                        case dom of
                            Public  -> 0
                            Private -> 1
                            _       -> error "Can only create a table for known privacy domain."
                ) xs


        hlengths  = map T.length tableHeader
        headerStr = mconcat tableHeader

        tableData = concatMap (zip types) tableRows

        boolData  = map snd $ filter (\x -> fst x == 0) tableData
        intData   = map snd $ filter (\x -> fst x == 1) tableData
        floatData = map snd $ filter (\x -> fst x == 2) tableData
        strData'  = map snd $ filter (\x -> fst x == 3) tableData

        vlengths = map T.length strData'
        strData  = mconcat strData'
tableGenerationCode _ _ _ = error "Invalid arguments passed to tableGenerationCode"

hdr :: [TopStatement]
hdr =
  [ Import "stdlib"
  , Import "shared3p"
  , Import "shared3p_string"
  , Import "shared3p_table_database"
  , Import "table_database"
  , P.Empty
  , Import "lp_essentials"
  , P.Empty
  , P.Empty
  ]
