module Test.Chuchu where

-- base
import Prelude
import Control.Monad
import System.Exit
import System.IO hiding (hPutStrLn)

-- text
import Data.Text hiding (concat)
import qualified Data.Text as T
import Data.Text.IO

-- hslogger
import System.Log.Logger

-- parsec
import Text.Parsec.Text
import Text.Parsec

-- chuchu
import Language.Gherkin

chuchuMain :: Chuchu -> FilePath -> IO ()
chuchuMain chuchu f
  = do
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    parsed <- parseFile f
    case parsed of
      (Right strs)
        -> do
          codes <- mapM (processStr chuchu) strs
          unless (and codes) exitFailure
      (Left e) -> error $ "Could not parse " ++ f ++ ": " ++ show e

type Chuchu = [(Step, [ChuchuParser], [Value] -> IO ())]
data ChuchuParser = CPT String | Number deriving Show
type Value = Int

processStr
  :: [(Step, [ChuchuParser], [Value] -> IO ())] -> (Step, Text) -> IO Bool
processStr chuchu str
  = do
    codes <- mapM (processStepRule str) chuchu
    if or codes
      then return True
      else do
        hPutStrLn stderr
          $ T.concat
            [pack "The step ",
              pack $ show str,
              pack
                " doesn't match any step definitions I know."]
        return False

processStepRule
  :: (Step, Text) -> (Step, [ChuchuParser], [Value] -> IO ()) -> IO Bool
processStepRule (strStep, str) (chuchuStep, cParser, action)
  | strStep == chuchuStep
    = case match cParser str of
      Left e
        -> do
          criticalM rootLoggerName
            $ "Could not match parser "
              ++ show cParser
              ++ " on string "
              ++ show str
              ++ ": "
              ++ show e
          return False
      Right parameters
        -> do
          criticalM rootLoggerName
            $ "Parser "
              ++ show cParser
              ++ " on string "
              ++ show str
              ++ " returned values "
              ++ show parameters
          action parameters
          return True
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
