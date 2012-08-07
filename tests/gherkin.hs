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
  = TestList
    [TestCase
        $ [(When, pack "I set the variable as 3 into the environment"),
            (Then, pack "the variable should have 3 on its content")]
          @=? fromRight
           (unsafePerformIO $ parseFile "tests/data/environment.feature"),
      TestCase
        $ [(Given, pack "that A is logged in"),
            (When, pack "he goes to his user page"),
            (Then, pack "he sees his user name"),
            (When, pack "he clicks \"Premium\""),
            (Then, pack "he sees that he is a premium user")]
          @=? fromRight
            (unsafePerformIO
              $ parseFile "tests/data/givenWhenGivenWhen.feature")]
