module Main where

import Relude
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Control.Lens

import Options.Applicative

import DatalogProgram as DP
import Language.SecreC.SCProgram as P
import Language.SecreC.SCExpr
import Language.SecreC.Builtin as BI
import Parser.DatalogParser
import Expr
import Table

data ProgramOpts = ProgramOpts
  { _poInFilePath :: Text
  , _poOutFilePath :: Text
  }

optParse :: Parser ProgramOpts
optParse = ProgramOpts 
  <$> strArgument (mconcat 
        [ metavar "INPUT"
        , help "Path to Privalog program"
        ])
  <*> strArgument (mconcat 
        [ metavar "OUTPUT"
        , value "createdb_out"
        , help "Output file path"
        ])
  

main :: IO ()
main =
  do
    let opts = info (optParse <**> helper) $ mconcat
          [ fullDesc
          , progDesc "Generate a SecreC program for setting up the database"
          ]
    args <- execParser opts
    let inFilePath = _poInFilePath args
    let outFilePath = _poOutFilePath args
    program' <- parseDatalogFromFile inFilePath
    let program = either (error . show) id program'
    createdb <- csvImportCode program
    let createdbStr = show . pretty $ createdb
    writeFile (toString outFilePath) createdbStr

csvImportCode :: DP.DatalogProgram -> IO SCProgram
csvImportCode dp = do
  let ds = "ds"
  let extPreds = dp ^.. DP.dpDBClauses
  tableData <- mapM (getTableData . view predName) extPreds
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
                            PPInt32   -> 1
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

