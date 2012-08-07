-- base
import System.Exit
import System.IO.Unsafe

-- text
import Data.Text

-- MissingH
import Data.Either.Utils

-- chuchu
import Language.Gherkin

-- HUnit
import Test.HUnit

main :: IO ()
main
  = do
    counts_ <- runTestTT tests
    if errors counts_ + failures counts_ > 0
      then exitFailure
      else exitSuccess

tests :: Test
tests
  = TestCase
    $ Gherkin
        []
        [pack "I set the variable as 3 into the environment"]
        [pack "the variable should have 3 on its content"]
      @=? fromRight
        (unsafePerformIO $ parseFile "tests/data/environment.feature")
