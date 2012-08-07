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
        $ Gherkin
            []
            (pack "Environment")
            [pack "In order to use the enviroment variables",
              pack
                $ "Programmers must be able to write and read environment "
                  ++ "variables"]
            [Scenario
              (pack "Write then read")
              [(When, pack "I set the variable as 3 into the environment"),
                (Then, pack "the variable should have 3 on its content")]]
          @=? fromRight
           (unsafePerformIO $ parseFile "tests/data/environment.feature"),
      TestCase
        $ Gherkin
            []
            (pack "Given When Given When")
            [pack "In order to write more complicated test cases",
              pack
                $ "Programmers must be able to write more than one Given When "
                  ++ "sequence"]
            [Scenario
              (pack "Given When Given When")
              [(Given, pack "that A is logged in"),
                (When, pack "he goes to his user page"),
                (Then, pack "he sees his user name"),
                (When, pack "he clicks \"Premium\""),
                (Then, pack "he sees that he is a premium user")]]
          @=? fromRight
            (unsafePerformIO
              $ parseFile "tests/data/givenWhenGivenWhen.feature"),
      TestCase
        $ Gherkin
            [pack "needs_calc", pack "nice_feature"]
            (pack "Division")
            [pack "In order to avoid silly mistakes",
              pack "Cashiers must be able to calculate a fraction"]
            [Scenario
              (pack "Regular numbers")
              [(Given, pack "that I have entered 3 into the calculator"),
                (Given, pack "that I have entered 2 into the calculator"),
                (When, pack "I press divide"),
                (Then, pack "the result should be 1.5 on the screen")]]
          @=? fromRight
            (unsafePerformIO
              $ parseFile "tests/data/calculator.feature")]
