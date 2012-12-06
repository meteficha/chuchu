import Control.Exception (handle)
import System.Environment (withArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import Test.Chuchu


defs :: Chuchu IO
defs = do
  Given "failing step" $ \() -> do
    fail "Error"


-- | This functions verifies if the behaviour of Chuchu was
-- correct. The correct behaviour is to have 'n' number of failed
-- scenarios (this number comes from the exit code). If no scenarios
-- fail or the number of failed scenarios is different than 'n' then
-- Chuchu failed.
scenariosShouldFail :: Int -> IO () -> IO ()
scenariosShouldFail n = handle exit
    where exit ExitSuccess                  = failWith 255
          exit (ExitFailure n') | n /= n'   = failWith n'
                                | otherwise = putStrLn "Okay, everything failed!"
          failWith n' = do
            putStrLn $ concat
              [ "\n"
              , show n
              , " `scenariosShouldFail` _: failure, exiting with code "
              , show n'
              , "." ]
            exitWith (ExitFailure n')


main :: IO ()
main = withArgs [ "tests/data/should_fail.feature" ] $
         2 `scenariosShouldFail` chuchuMain defs id

