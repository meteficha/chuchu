module Test.Chuchu where

-- base
import Prelude
import Control.Applicative
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
chuchuMain (Chuchu gStep wStep tStep) f
  = do
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    parsed <- parseFile f
    case parsed of
      (Right (Gherkin gStrs wStrs tStrs))
        -> do
          codes
            <- mapM processStep [(gStep, gStrs), (wStep, wStrs), (tStep, tStrs)]
          unless (and codes) exitFailure
      (Left e) -> error $ "Could not parse " ++ f ++ ": " ++ show e

data Chuchu
  = Chuchu
    {cGiven :: [([ChuchuParser], [Value] -> IO ())],
      cWhen :: [([ChuchuParser], [Value] -> IO ())],
      cThen :: [([ChuchuParser], [Value] -> IO ())]}

data Step = Given | When | Then
data ChuchuParser = CPT String | Number deriving Show
type Value = Int

processStep :: ([([ChuchuParser], [Value] -> IO ())], [Text]) -> IO Bool
processStep (step, strs) = or <$> mapM (processStr step) strs

processStr :: [([ChuchuParser], [Value] -> IO ())] -> Text -> IO Bool
processStr step str
  = do
    codes <- mapM (processStepRule str) step
    if or codes
      then return True
      else do
        hPutStrLn stderr
          $ T.concat
            [pack "The step ",
              str,
              pack
                " doesn't match any step definitions I know."]
        return False

processStepRule :: Text -> ([ChuchuParser], [Value] -> IO ()) -> IO Bool
processStepRule str (cParser, action)
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
