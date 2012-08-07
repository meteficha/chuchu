-- base
import Control.Applicative
import Data.Maybe

-- unix
import System.Posix.Env

-- chuchu
import Language.Gherkin
import Test.Chuchu

-- HUnit
import Test.HUnit

enterNumber :: Int -> IO ()
enterNumber n
  = do
    putStrLn "setting..."
    setEnv "environment" (show n) True

getNumber :: IO Int
getNumber
  = do
    putStrLn "getting..."
    read <$> fromJust <$> getEnv "environment"

defs :: Chuchu
defs = [
  (When, [CPT "I set the variable as ", Number, CPT " into the environment"],
    \ [n] -> enterNumber n),
  (Then, [CPT "the variable should have ", Number, CPT " on its content"],
    \ [n]
      -> do
        putStrLn "getting...1"
        d <- getNumber
        n @=? d)]

main :: IO ()
main = chuchuMain defs "tests/data/environment.feature"
