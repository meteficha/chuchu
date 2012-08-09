-- Copyright 2012 Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Test.Chuchu (chuchuMain, module Test.Chuchu.Parser) where

-- base
import Control.Applicative
import Control.Monad
import System.Environment
import System.Exit
import System.IO

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- parsec
import Text.Parsec
import Text.Parsec.Text ()

-- cmdargs
import System.Console.CmdArgs

-- abacate
import Language.Abacate

-- chuchu
import Test.Chuchu.Parser

chuchuMain :: MonadIO m => Chuchu m -> (m () -> IO ()) -> IO ()
chuchuMain cc runMIO
  = do
    path <- getPath
    parsed <- parseFile path
    case parsed of
      (Right abacate)
        -> runMIO
          $ runReaderT
            (do
              code <- processAbacate abacate
              unless code $ liftIO exitFailure)
            cc
      (Left e) -> error $ "Could not parse " ++ path ++ ": " ++ show e

type CM m a = ReaderT (Chuchu m) m a

processAbacate :: MonadIO m => Abacate -> CM m Bool
processAbacate feature
  = do
    bCode
      <- case fBackground feature of
        Nothing -> return True
        Just background -> processBasicScenario background
    feCode <- processFeatureElements $ fFeatureElements feature
    return $ bCode && feCode

processFeatureElements :: MonadIO m => FeatureElements -> CM m Bool
processFeatureElements featureElements
  = do
    codes <- mapM processFeatureElement featureElements
    return $ and codes

processFeatureElement :: MonadIO m => FeatureElement -> CM m Bool
processFeatureElement (FESO _)
  = liftIO (hPutStrLn stderr "Scenario Outlines are not supported yet.")
    >> return False
processFeatureElement (FES sc) = processBasicScenario $ scBasicScenario sc

processBasicScenario :: MonadIO m => BasicScenario -> CM m Bool
processBasicScenario = processSteps . bsSteps

processSteps :: MonadIO m => Steps -> CM m Bool
processSteps steps
  = do
    codes <- mapM processStep steps
    return $ and codes

processStep :: MonadIO m => Step -> CM m Bool
processStep step
  = do
    cc <- ask
    result <- lift $ runPT cc () "processStep" $ stBody step
    case result of
      Left e
        -> do
          liftIO
            $ hPutStrLn stderr
            $ "The step "
              ++ show (stBody step)
              ++ " doesn't match any step definitions I know."
              ++ show e
          return False
      Right () -> return True

data Options
  = Options {file_ :: FilePath}
    deriving (Eq, Show, Typeable, Data)

getPath :: IO FilePath
getPath
  = do
    progName <- getProgName
    file_
      <$> cmdArgs
        (Options (def &= typ "PATH" &= argPos 0)
          &= program progName
          &= details
            ["Run test scenarios specified on the abacate file at PATH."])
