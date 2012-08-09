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

module Test.Chuchu where

-- base
import Control.Applicative
import Control.Monad
import System.Exit
import System.IO

-- text
import Data.Text hiding (concat)

-- parsec
import Text.Parsec.Text
import Text.Parsec

-- abacate
import Language.Abacate

chuchuMain :: Chuchu -> FilePath -> IO ()
chuchuMain chuchu f
  = do
    parsed <- parseFile f
    case parsed of
      (Right feature)
        -> do
          bCode
            <- case fBackground feature of
              Nothing -> return True
              Just background -> processBasicScenario chuchu background
          feCode <- processFeatureElements chuchu $ fFeatureElements feature
          unless (bCode && feCode) exitFailure
      (Left e) -> error $ "Could not parse " ++ f ++ ": " ++ show e

type Chuchu = [(StepKeyword, [ChuchuParser], [Value] -> IO ())]
data ChuchuParser = CPT String | Number deriving Show
type Value = Int

processFeatureElements :: Chuchu -> FeatureElements -> IO Bool
processFeatureElements chuchu featureElements
  = and <$> mapM (processFeatureElement chuchu) featureElements

processFeatureElement :: Chuchu -> FeatureElement -> IO Bool
processFeatureElement _ (FESO _)
  = hPutStrLn stderr "Scenario Outlines are not supported yet." >> return False
processFeatureElement chuchu (FES sc)
  = processBasicScenario chuchu $ scBasicScenario sc

processBasicScenario :: Chuchu -> BasicScenario -> IO Bool
processBasicScenario chuchu = processSteps chuchu . bsSteps

processSteps :: Chuchu -> Steps -> IO Bool
processSteps chuchu steps = and <$> mapM (processStep chuchu) steps

processStep :: Chuchu -> Step -> IO Bool
processStep chuchu step
  = do
    codes <- mapM (tryMatchStep step) chuchu
    if or codes
      then return True
      else do
        hPutStrLn stderr
          $ "The step "
            ++ show (stBody step)
            ++ " doesn't match any step definitions I know."
        return False

tryMatchStep
  :: Step -> (StepKeyword, [ChuchuParser], [Value] -> IO ()) -> IO Bool
tryMatchStep step (chuchuStep, cParser, action)
  | stStepKeyword step == chuchuStep
    = case match cParser $ stBody step of
      Left _ -> return False
      Right parameters -> action parameters >> return True
  | otherwise = return False

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
