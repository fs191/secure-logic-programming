{-# LANGUAGE OverloadedStrings #-}

module Swipl 
  ( runDatalogFromFile
  , runDatalogProgram
  ) where

import Shelly
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import DatalogProgram

runDatalogFromFile :: FilePath -> IO [Text]
runDatalogFromFile p = shelly $ runDatalogFromFile' p

runDatalogFromFile' :: FilePath -> Sh [Text]
runDatalogFromFile' p = silently $
  do
    let _path = T.pack p
    res <- run "swipl" 
      [ "-g"
      , "goal(X,Y),maplist(writeln,Y)"
      , "-t"
      , "halt"
      , _path
      ]
    return $ T.lines res

runDatalogProgram :: DatalogProgram -> IO [Text]
runDatalogProgram dp = shelly $ withTmpDir action
  where 
    action :: FilePath -> Sh [Text]
    action tmp =
      do
        let _path = tmp <> "/dprog"
        writefile _path . T.pack . show $ pretty dp
        runDatalogFromFile' _path

