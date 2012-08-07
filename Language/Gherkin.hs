module
  Language.Gherkin
  (parseFile, parseGherkin, Gherkin (..))
  where

-- base
import Prelude hiding (readFile, unlines)
import Control.Applicative hiding (many, (<|>), optional)
import Control.Monad

-- text
import Data.Text
import Data.Text.IO

-- parsec
import Text.Parsec.Text
import Text.Parsec

parseFile :: FilePath -> IO (Either ParseError Gherkin)
parseFile path = parseGherkin <$> readFile path

parseGherkin :: Text -> Either ParseError Gherkin
parseGherkin s =
  do
    stripped <- parse stripComments "stripComments" s
    parse gherkin "gherkin" stripped

data Gherkin =
  Gherkin
    {gGiven :: [Text],
      gWhen :: [Text],
      gThen :: [Text]}
  deriving (Eq, Show)

stripComments :: GenParser st Text
stripComments
  = unlines
    <$> many
      (try
        $ do
          bef <- many1 (noneOf "#")
          optional $ char '#' >> void rol
          return $ pack bef)

gherkin :: GenParser st Gherkin
gherkin
 = do
   void $ manyTill anyChar $ try $ newline >> newline
   void rol
   pGiven <- step "Given"
   pWhen <- step "When"
   pThen <- step "Then"
   return $ Gherkin pGiven pWhen pThen

step :: String -> GenParser st [Text]
step name
  = option []
    $ try
    $ do
      spaces
      void $ string name
      spaces
      text <- rol
      rest <- many $ try $ spaces >> string "And" >> spaces >> rol
      return $ strip text : rest

rol :: GenParser st Text
rol = pack <$> manyTill anyChar (void newline <|> eof)
