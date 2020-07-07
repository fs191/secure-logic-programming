{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Swipl 
  ( runDatalogFromFile
  , runDatalogProgram
  , preservesSemantics
  , preservesSemanticsDB
  , runsSuccessfully
  , runsSuccessfullyDB
  , doesNotRun
  ) where

import Shelly

import Control.Lens

import Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Control.Exception

import Test.Hspec

import Parser.DatalogParser
import DatalogProgram
import Expr
import Rule

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
          , "forall(goal(X,Y), writeln(Y))"
          , "-t"
          , "halt"
          , _path
          ]
    _source <- liftIO $ readFile p
    let _num = ((("\n"++) . (++"\t") . show) <$> ([1..] :: [Int]))
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
preservesSemantics f p = preservesSemanticsDB f p []

preservesSemanticsDB
  :: (DatalogProgram -> DatalogProgram) 
  -> String 
  -> [Expr]
  -> Spec
preservesSemanticsDB f p db = it desc $
  do
    _prog <- parseDatalogFromFile p
    _pre  <- runDatalogProgram $ insertDB db _prog
    _post <- runDatalogProgram . insertDB db $ f _prog
    S.fromList _pre `shouldBe` S.fromList _post
  where
    desc = "preserves semantics of " <> p

runsSuccessfully :: String -> [Text] -> Spec
runsSuccessfully p res = runsSuccessfullyDB p res []

runsSuccessfullyDB :: String -> [Text] -> [Expr] -> Spec
runsSuccessfullyDB p res db = it desc $
  do
    _prog <- parseDatalogFromFile $ p
    _res <- runDatalogProgram $ insertDB db _prog
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

insertDB :: [Expr] -> DatalogProgram -> DatalogProgram
insertDB db x = x & dpRules %~ (<> _dbRules)
  where
    _dbRules :: [Rule]
    _dbRules = [fact _n _as | Pred _ _n _as <- db]


