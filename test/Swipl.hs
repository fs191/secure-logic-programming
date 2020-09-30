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
  , compilesSuccessfully
  ) where

import Shelly

import Control.Lens

import Data.Set as S
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T

import Control.Exception

import Test.Hspec

import Parser.DatalogParser
import DatalogProgram
import Expr
import Rule

import Transform
import PreProcessing
import PostProcessing
import TypeInference
import Adornment
import Language.SecreC

data SwiplException
  = SwiplException String
  | SecreCException Text
  deriving (Exception, Eq)

instance Show SwiplException where
  show (SwiplException s) = "Failed to run swipl program:\n" ++ s
  show (SecreCException s) = T.unpack $
    "Failed to compile SecreC program:\n" <> s

runDatalogFromFile :: FilePath -> IO (Either SwiplException [Text])
runDatalogFromFile p = shelly $ runDatalogFromFile' p

runDatalogFromFile' :: FilePath -> Sh (Either SwiplException [Text])
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
      (\e -> return . Left . SwiplException $ _source <> "\n\n" <> show e) $
      Right <$> _action
    return $ T.lines <$> res

-- | Runs the program `dp` using swipl. 
runDatalogProgram :: DatalogProgram -> IO (Either SwiplException [Text])
runDatalogProgram dp = shelly $ withTmpDir action
  where 
    action :: FilePath -> Sh (Either SwiplException [Text])
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
    case _pre of
      Left e -> expectationFailure $ show e
      Right _ -> 
        if (S.fromList <$> _pre) == (S.fromList <$> _post)
          then return ()
          else expectationFailure $ 
            "\nORIGINAL:\n" <> (show $ prolog _prog)     <> "\n\n" <>
            "MODIFIED:\n"   <> (show . prolog $ f _prog) <> "\n\n" <>
            "expected:\n"   <> show _pre                 <> "\n\n" <>
            "got:\n"        <> show _post
  where
    desc = "preserves semantics of " <> p

runsSuccessfully :: String -> (DatalogProgram -> DatalogProgram) -> [Text] -> Spec
runsSuccessfully n p res = runsSuccessfullyDB n p res []

runsSuccessfullyDB :: String -> (DatalogProgram -> DatalogProgram) -> [Text] -> [Expr] -> Spec
runsSuccessfullyDB n p res db = it desc $
  do
    _prog <- parseDatalogFromFile n
    let p' = insertDB db . p $ _prog
    _res <- runDatalogProgram p'
    if (S.fromList <$> _res) == (Right $ S.fromList res)
      then return ()
      else expectationFailure $ 
        "\nORIGINAL:\n" <> (show $ prolog _prog) <> "\n\n" <>
        "MODIFIED:\n" <> (show $ prolog p') <> "\n\n" <>
        "expected: " <> show res <> "\n" <>
        "got: " <> show _res
  where
    desc = "evaluating " <> n <> " outputs " <> show res

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

compileSC :: String -> Sh (Either SwiplException Text)
compileSC file = errExit False $ withTmpDir act
  where
    act tmp = 
      do
        _prog <- liftIO $ parseDatalogFromFile file
        let sc = secrecCode . typeInference . postProcess . deriveAllGroundRules 2 . preProcess $ adornProgram _prog
        cp "SecreC/lp_essentials.sc" tmp
        let _path = tmp <> "prog.sc"
        let src = T.pack . show $ pretty sc
        let enumf :: (T.Text, Int) -> Text
            enumf (l, i) = (T.pack $ show i) <> "\t| " <> l
        let srcNum = fmap enumf (T.lines src `zip` [1..])
        writefile _path src
        cd tmp
        res <- run "scc" [T.pack _path, "-I", T.pack tmp, "-o", "out.sb"]
        err <- lastStderr
        resCode <- lastExitCode
        let ex = err <> "\n" <> res <> "\nSOURCE:\n" <> T.intercalate "\n" srcNum
        case resCode of
          0 -> return $ Right res
          _ -> return . Left $ SecreCException ex

compilesSuccessfully :: String -> Spec
compilesSuccessfully file = it desc $ do
  let act =
        do
          res <- shelly . silently $ compileSC file
          case res of
            Left ex -> throw ex
            Right _ -> return ()
  act `shouldReturn` ()
  where
    desc = "Translates " ++ file ++ " to SecreC successfully." 

