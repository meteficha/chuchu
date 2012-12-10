-- |
-- Module      :  Test.Chuchu
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (DeriveDataTypeable)
--
-- Chuchu is a system similar to Ruby's Cucumber for Behaviour Driven
-- Development.  It works with a language similar to Cucumber's Gherkin, which
-- is parsed using package abacate.
--
-- This module provides the main function for a test file based on Behaviour
-- Driven Development for Haskell.
--
-- Example for a Stack calculator:
--
-- @calculator.feature@:
--
-- @
--Feature: Division
--  In order to avoid silly mistakes
--  Cashiers must be able to calculate a fraction
--
--  Scenario: Regular numbers
--    Given that I have entered 3 into the calculator
--    And that I have entered 2 into the calculator
--    When I press divide
--    Then the result should be 1.5 on the screen
-- @
--
-- @calculator.hs@:
--
-- @
--import Control.Applicative
--import Control.Monad.IO.Class
--import Control.Monad.Trans.State
--import Test.Chuchu
--import Test.HUnit
--
--type CalculatorT m = StateT \[Double\] m
--
--enterNumber :: Monad m => Double -> CalculatorT m ()
--enterNumber = modify . (:)
--
--getDisplay :: Monad m => CalculatorT m Double
--getDisplay
--  = do
--    ns <- get
--    return $ head $ ns ++ [0]
--
--divide :: Monad m => CalculatorT m ()
--divide = do
--  (n1:n2:ns) <- get
--  put $ (n2 / n1) : ns
--
--defs :: Chuchu (CalculatorT IO)
--defs
--  = do
--    Given
--      (\"that I have entered \" *> number <* \" into the calculator\")
--      enterNumber
--    When \"I press divide\" $ const divide
--    Then (\"the result should be \" *> number <* \" on the screen\")
--      $ \\n
--        -> do
--          d <- getDisplay
--          liftIO $ d \@?= n
--
--main :: IO ()
--main = chuchuMain defs (\`evalStateT\` [])
-- @

module
  Test.Chuchu
  (chuchuMain, module Test.Chuchu.Types, module Test.Chuchu.Parser)
  where

import Control.Applicative ((<$>), Applicative((<*>)))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Data.Either (partitionEithers)
import Language.Abacate hiding (StepKeyword (..))
import System.Console.CmdArgs
import System.Environment (getProgName)
import System.Exit (exitFailure, exitWith, ExitCode(ExitFailure))
import qualified Control.Exception.Lifted as E
import qualified Data.IORef as I
import qualified Text.Parsec as P

import Test.Chuchu.Types
import Test.Chuchu.Parser
import Test.Chuchu.OutputPrinter


----------------------------------------------------------------------


-- | The main function for the test file. It expects one or more
-- @.feature@ file as parameters on the command line. If you want to
-- use it inside a library, consider using 'withArgs'.
chuchuMain :: (MonadBaseControl IO m, MonadIO m, Applicative m) =>
              Chuchu m -> (m () -> IO ()) -> IO ()
chuchuMain stepDefinitions runMIO = do
  listOfPaths <- getPaths
  parsedFiles <- mapM parseFile listOfPaths
  let result = partitionEithers parsedFiles
  case result of
    -- no error in parsing, execute all files
    ([], filesToExecute) -> do
      rets <- concat <$> mapM (processAbacate stepDefinitions runMIO) filesToExecute
      let n = length $ filter not rets
      unless (n == 0) $ exitWith (ExitFailure (min 255 n)) -- the size of a Unix error code is 1 byte
    -- there were errors, print them and execute nothing
    (filesWithError, _)  -> do
      warn "Could not parse the following files: "
      mapM_ (warn . flip (++) "\n" . show) filesWithError
      exitFailure


----------------------------------------------------------------------


-- | An execution plan for a scenario.  Currently just a simple
-- record with a background (optional) and a scenario.
data ExecutionPlan =
  ExecutionPlan
    { epBackground :: Maybe Background
    , epScenario   :: FeatureElement
    }
  deriving (Show)


-- | Creates an execution plan for a feature.
createExecutionPlans :: Abacate -> [ExecutionPlan]
createExecutionPlans feature =
  ExecutionPlan (fBackground feature) `map` fFeatureElements feature


----------------------------------------------------------------------


-- | Monad used when executing a feature's scenario.  'ReaderT'
-- is used to carry along the step parser.
type Execution m a = ReaderT (ParseStep m) m a


-- | A function that parses a step and, if successful, returns
-- the corresponding action to be executed.
type ParseStep m = Step -> Either P.ParseError (m ())


-- | Run the 'Execution' monad.
runExecution :: (MonadIO m, Applicative m) =>
                Chuchu m -> (m () -> IO ()) -> Execution m () -> IO ()
runExecution stepDefinitions runMIO act = runMIO $ runReaderT act parseStep
  where parseStep = P.parse (runChuchu stepDefinitions) "Step definitions" . stBody


----------------------------------------------------------------------


-- | Process a whole Abacate file, that is, a whole feature.
-- Runs each background+scenario combination on a different
-- instance of the 'Execution' monad.
processAbacate :: (MonadBaseControl IO m, MonadIO m, Applicative m) =>
                  Chuchu m
               -> (m () -> IO ())
               -> Abacate
               -> IO [Bool]
processAbacate stepDefinitions runMIO feature = do
  -- Print feature description.
  putDoc $ describeAbacate feature

  -- Execute features.
  let plans = createExecutionPlans feature
  retVar <- liftIO $ I.newIORef []
  let addRet ret = liftIO $ I.modifyIORef retVar (ret:)
  mapM_ (runExecution stepDefinitions runMIO . (>>= addRet) . processExecutionPlan) plans
  reverse <$> liftIO (I.readIORef retVar)


-- | Process a single execution plan, a combination of
-- background+scenario, inside the 'Execution' monad.
processExecutionPlan :: (MonadBaseControl IO m, MonadIO m, Applicative m) =>
                        ExecutionPlan -> Execution m Bool
processExecutionPlan (ExecutionPlan mbackground scenario) = do
  liftIO $ putStrLn "" -- empty line (TODO: move into OutputPrinter somehow)
  (&&) <$> maybe (return True) (processBasicScenario BackgroundKind) mbackground
       <*> processFeatureElement scenario


----------------------------------------------------------------------


processFeatureElement :: (MonadBaseControl IO m, MonadIO m, Applicative m) =>
                         FeatureElement -> Execution m Bool
processFeatureElement (FESO _)
  = warn "Scenario Outlines are not supported yet." >> return False
processFeatureElement (FES sc) =
  processBasicScenario (ScenarioKind $ scTags sc) $ scBasicScenario sc


processBasicScenario :: (MonadBaseControl IO m, MonadIO m, Applicative m) =>
                        BasicScenarioKind -> BasicScenario -> Execution m Bool
processBasicScenario kind scenario = do
  putDoc $ describeBasicScenario kind scenario
  processSteps (bsSteps scenario)


----------------------------------------------------------------------


processSteps :: (MonadBaseControl IO m, MonadIO m, Applicative m) => Steps -> Execution m Bool
processSteps steps = mapShortCircuitM processStep steps


mapShortCircuitM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
mapShortCircuitM _ []     = return True
mapShortCircuitM f (x:xs) = do
  ret <- f x
  if ret then mapShortCircuitM f xs
         else return False


-- | Executes the parser of each step and prints the result on the
-- screen.
processStep :: (MonadBaseControl IO m, MonadIO m, Applicative m) => Step -> Execution m Bool
processStep step = do
  parseStep <- ask
  case parseStep step of
    Left e -> do
      putDoc $ describeStep UnknownStep step
      liftIO $ warn $ concat [ "The step "
                             , show (stBody step)
                             , " doesn't match any step definitions I know."
                             , show e ]
      return False
    Right m -> do
      r <- E.catches (lift m >> return SuccessfulStep)
             [ E.Handler $ \(e :: E.AsyncException) -> E.throw (e :: E.AsyncException)
             , E.Handler $ \(_ :: E.SomeException)  -> return FailedStep ]
      putDoc (describeStep r step)
      return (r == SuccessfulStep)


----------------------------------------------------------------------


data Options
  = Options {file_ :: [FilePath]}
    deriving (Eq, Show, Typeable, Data)


-- Gets the feature files as arguments from the command-line.
getPaths :: IO [FilePath]
getPaths = do
  progName <- getProgName
  file_ <$> cmdArgs (Options (def &= typ "PATH" &= args)
                     &= program progName
                     &= details ["Run one or more abacate files."])
