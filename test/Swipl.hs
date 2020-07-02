{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Swipl 
  ( runDatalogFromFile
  , runDatalogProgram
  , preservesSemantics
  , runsSuccessfully
  , doesNotRun
  ) where

import Shelly

import Data.Text (Text)
import qualified Data.Text as T

import Control.Exception

import Test.Hspec

import Parser.DatalogParser
import DatalogProgram

data SwiplException
  = SwiplException String
  deriving (Exception)

instance Show SwiplException where
  show (SwiplException s) = "Failed to run swipl program: " ++ s

runDatalogFromFile :: FilePath -> IO [Text]
runDatalogFromFile p = shelly $ runDatalogFromFile' p

runDatalogFromFile' :: FilePath -> Sh [Text]
runDatalogFromFile' p = silently $
  do
    let _path = T.pack p
    let _action = run "swipl" 
          [ "-g"
          , "goal(X,Y),maplist(writeln,Y)"
          , "-t"
          , "halt"
          , _path
          ]
    _source <- liftIO $ readFile p
    let _num = ((("\n"++) . (++"\t") . show) <$> [1..])
    let _numSource = concat $ (uncurry (++)) <$> _num `zip` lines _source
    res <- handleany_sh 
      (\e -> throw $ SwiplException (show e ++ "\nSource:\n" ++ _numSource)) 
      _action
    return $ T.lines res

-- | Runs the program `dp` using swipl. 
runDatalogProgram :: DatalogProgram -> IO [Text]
runDatalogProgram dp = shelly $ withTmpDir action
  where 
    action :: FilePath -> Sh [Text]
    action tmp =
      do
        let _path = tmp <> "/dprog"
        writefile _path . T.pack . show $ prolog dp
        runDatalogFromFile' _path

preservesSemantics 
  :: (DatalogProgram -> DatalogProgram) 
  -> String 
  -> Spec
preservesSemantics f p = it desc $
  do
    _prog <- parseDatalogFromFile p
    _pre  <- runDatalogProgram _prog
    _post <- runDatalogProgram (f _prog)
    _pre `shouldBe` _post
  where
    desc = "preserves semantics of " <> p

runsSuccessfully :: String -> [Text] -> Spec
runsSuccessfully p res = it desc $
  do
    _prog <- parseDatalogFromFile p
    _res <- runDatalogProgram _prog
    _res `shouldBe` res
  where
    desc = "evaluating " <> p <> " outputs " <> show res

doesNotRun :: String -> Spec
doesNotRun p = it desc $
  do
    _prog <- parseDatalogFromFile p
    runDatalogProgram _prog `shouldThrow` anyException
  where
    desc = p <> " throws an exception"
