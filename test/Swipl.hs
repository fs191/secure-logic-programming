{-# LANGUAGE DeriveAnyClass #-}

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

import Relude

import Shelly

import Control.Lens

import Data.Set as S
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T

import Control.Exception

import Test.Hspec

import qualified Text.Show

import Parser.DatalogParser
import DatalogProgram
import Expr
import Rule

import Language.SecreC
import Translator

data SwiplException
  = SwiplException Text
  | SecreCException Text
  deriving (Exception, Eq)

instance Show SwiplException where
  show (SwiplException s) = "Failed to run swipl program:\n" <> toString s
  show (SecreCException s) = "Failed to compile SecreC program:\n" <> toString s

runDatalogFromFile :: Text -> IO (Either SwiplException [Text])
runDatalogFromFile p = shelly $ runDatalogFromFile' p

runDatalogFromFile' :: Text -> Sh (Either SwiplException [Text])
runDatalogFromFile' p = silently $
  do
    let _path = p
    let _action = run "swipl" 
          [ "-g"
          , "forall(goal(X,Y), writeln(Y))"
          , "-t"
          , "halt"
          , _path
          ]
    _source <- liftIO $ readFileText (toString p)
    let _num = ((("\n"<>) . (<>"\t") . show) <$> ([1..] :: [Int]))
    let _numSource = mconcat $ (uncurry (<>)) <$> _num `zip` lines _source
    res <- handleany_sh 
      (\e -> return . Left . SwiplException $ enumerateLines' _source <> "\n\n" <> show e) $
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
        writefile _path . show $ prolog dp
        runDatalogFromFile' $ toText _path

preservesSemantics 
  :: (DatalogProgram -> IO DatalogProgram) 
  -> Text 
  -> Spec
preservesSemantics f p = preservesSemanticsDB f p []

preservesSemanticsDB
  :: (DatalogProgram -> IO DatalogProgram) 
  -> Text 
  -> [Expr]
  -> Spec
preservesSemanticsDB f p db = it (toString desc) $
  do
    _prog <- parseDatalogFromFile p
    _pre  <- runDatalogProgram $ insertDB db _prog
    _fprog <- f _prog
    _post <- runDatalogProgram $ insertDB db _fprog
    case _pre of
      Left e -> expectationFailure $ show e
      Right _ -> 
        if (S.fromList <$> _pre) == (S.fromList <$> _post)
          then return ()
          else expectationFailure . toString $ 
            "\nORIGINAL:\n" <> (enumerateLines' . show $ prolog _prog)     <> "\n\n" <>
            "MODIFIED:\n"   <> (enumerateLines' . show $ prolog _fprog) <> "\n\n" <>
            "expected:\n"   <> show _pre                 <> "\n\n" <>
            "got:\n"        <> show _post
  where
    desc = "preserves semantics of " <> p

runsSuccessfully :: Text -> (DatalogProgram -> IO DatalogProgram) -> [Text] -> Spec
runsSuccessfully n p res = runsSuccessfullyDB n p res []

runsSuccessfullyDB :: Text -> (DatalogProgram -> IO DatalogProgram) -> [Text] -> [Expr] -> Spec
runsSuccessfullyDB n p res db = it (toString desc) $
  do
    _prog <- parseDatalogFromFile n
    _fprog <- p _prog
    let p' = insertDB db _fprog
    _res <- runDatalogProgram p'
    if (S.fromList <$> _res) == (Right $ S.fromList res)
      then return ()
      else expectationFailure . toString $ 
        "\nORIGINAL:\n" <> (enumerateLines' . show $ prolog _prog) <> "\n\n" <>
        "MODIFIED:\n" <> (enumerateLines' . show $ prolog p') <> "\n\n" <>
        "expected: " <> show res <> "\n" <>
        "got: " <> show _res
  where
    desc = "evaluating " <> n <> " outputs " <> show res

doesNotRun :: Text -> Spec
doesNotRun p = it (toString desc) $
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

compileSC :: Text -> Int -> Sh (Either SwiplException Text)
compileSC file iterations = errExit False $ withTmpDir act
  where
    act tmp = 
      do
        _prog <- liftIO $ parseDatalogFromFile file
        let conf = TranslatorConfig iterations
        trans <- liftIO $ process conf _prog
        let sc = secrecCode trans
        cp "SecreC/lp_essentials.sc" tmp
        let _path = tmp <> "prog.sc"
        let src = show $ pretty sc
        let srcNum = enumerateLines src
        writefile (toString _path) src
        cd tmp
        res <- run "scc" 
                 [ toText _path
                 , "-I"
                 , toText tmp
                 , "-I"
                 , "/usr/lib/sharemind/stdlib"
                 , "-o"
                 , "out.sb"
                 ]
        err <- lastStderr
        resCode <- lastExitCode
        let ex = err <> "\n" <> res <> "\nSOURCE:\n" <> srcNum
        case resCode of
          0 -> return $ Right res
          _ -> return . Left $ SecreCException ex

compilesSuccessfully :: Text -> Int -> Spec
compilesSuccessfully file iterations = it (toString desc) $ do
  let act =
        do
          res <- shelly . silently $ compileSC file iterations
          case res of
            Left ex -> throw ex
            Right _ -> return ()
  act `shouldReturn` ()
  where
    desc = "Translates " <> file <> " to SecreC successfully." 

enumerateLines' :: Text -> Text
enumerateLines' = enumerateLines

enumerateLines :: Text -> Text
enumerateLines s = T.intercalate "\n" $ enumf <$> T.lines s `zip` [1..]
  where enumf :: (Text, Int) -> Text
        enumf (l, i) = show i <> "\t| " <> l
