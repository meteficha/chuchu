-- |
-- Module      :  Test.Chuchu
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (DeriveDataTypeable)
--
-- | This module provides the main function for a test file based on Behaviour
-- Driven Development for Haskell.
--
-- Example for a Stack calculator:
--
-- calculator.feature:
--
-- @
-- Feature: Division
--   In order to avoid silly mistakes
--   Cashiers must be able to calculate a fraction
--
--   Scenario: Regular numbers
--     Given that I have entered 2 into the calculator
--     And that I have entered 3 into the calculator
--     When I press divide
--     Then the result should be 1.5 on the screen
-- @
--
-- calculator.hs
--
-- @
-- import Control.Applicative
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.State
-- import Test.Chuchu
-- import Test.HUnit
--
-- type CalculatorT m = StateT [Double] m
--
-- enterNumber :: Monad m => Double -> CalculatorT m ()
-- enterNumber = modify . (:)
--
-- getDisplay :: Monad m => CalculatorT m Double
-- getDisplay
--   = do
--     ns <- get
--     return $ head $ ns ++ [0]
--
-- divide :: Monad m => CalculatorT m ()
-- divide = do
--   (n1:n2:ns) <- get
--   put $ (n1 / n2) : ns
--
-- defs :: Chuchu (CalculatorT IO)
-- defs
--   = do
--     Given
--       ("that I have entered " *> number <* " into the calculator")
--       enterNumber
--     When "I press divide" $ const divide
--     Then ("the result should be " *> number <* " on the screen")
--       $ \n
--         -> do
--           d <- getDisplay
--           liftIO $ d @?= n
--
-- main :: IO ()
-- main = chuchuMain defs (`evalStateT` [])
-- @

module
  Test.Chuchu
  (chuchuMain, module Test.Chuchu.Types, module Test.Chuchu.Parser)
  where

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
import Text.Parsec.Text

-- cmdargs
import System.Console.CmdArgs

-- abacate
import Language.Abacate hiding (StepKeyword (..))

-- chuchu
import Test.Chuchu.Types
import Test.Chuchu.Parser

-- | The main function for the test file.  It expects the @.feature@ file as the
-- first parameter on the command line.  If you want to use it inside a library,
-- consider using 'withArgs'.
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
          $ runChuchu cc
      (Left e) -> error $ "Could not parse " ++ path ++ ": " ++ show e

type CM m a = ReaderT (Parser (m ())) m a

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
    case parse cc "processStep" $ stBody step of
      Left e
        -> do
          liftIO
            $ hPutStrLn stderr
            $ "The step "
              ++ show (stBody step)
              ++ " doesn't match any step definitions I know."
              ++ show e
          return False
      Right m -> lift m >> return True

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
