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

module Test.Chuchu (chuchuMain, Chuchu, ChuchuParser (..), Value) where

-- base
import Control.Applicative
import Control.Monad
import System.Environment
import System.Exit
import System.IO

-- text
import Data.Text hiding (concat)

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- parsec
import Text.Parsec.Text
import Text.Parsec

-- cmdargs
import System.Console.CmdArgs

-- abacate
import Language.Abacate

chuchuMain :: MonadIO m => Chuchu m -> (m () -> IO ()) -> IO ()
chuchuMain chuchu runMIO
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
            chuchu
      (Left e) -> error $ "Could not parse " ++ path ++ ": " ++ show e

type Chuchu m = [([ChuchuParser], [Value] -> m ())]
data ChuchuParser = CPT String | Number deriving (Eq, Show)
type Value = Int
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
    chuchu <- ask
    codes <- lift $ mapM (tryMatchStep step) chuchu
    if or codes
      then return True
      else do
        liftIO
          $ hPutStrLn stderr
          $ "The step "
            ++ show (stBody step)
            ++ " doesn't match any step definitions I know."
        return False

tryMatchStep :: MonadIO m => Step -> ([ChuchuParser], [Value] -> m ()) -> m Bool
tryMatchStep step (cParser, action)
  = case match cParser $ stBody step of
    Left _ -> return False
    Right parameters -> action parameters >> return True

match :: [ChuchuParser] -> Text -> Either ParseError [Value]
match p = parse (pMatch p) "match"

pMatch :: [ChuchuParser] -> GenParser st [Value]
pMatch [] = return []
pMatch (CPT s : p) = string s >> pMatch p
pMatch (Number : p)
  = do
    d <- many1 digit
    rest <- pMatch p
    return $ read d : rest

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
