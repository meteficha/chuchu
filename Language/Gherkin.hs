module
  Language.Gherkin
  (parseFile, parseGherkin, Gherkin, Step (..))
  where

-- base
import Prelude hiding (readFile, unlines, last, init)
import Control.Applicative hiding (many, (<|>), optional)
import Control.Monad

-- text
import Data.Text hiding (map, concat)
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

type Gherkin = [(Step, Text)]
data Step = Given | When | Then deriving (Eq, Show, Bounded, Enum)

stripComments :: GenParser st Text
stripComments
    -- Remove last \n
  = init
    <$> unlines
    <$> many
      (try
        $ do
          bef <- many1 (noneOf "#")
          optional $ char '#' >> void rol
          let tbef = pack bef
          return $ if last tbef == '\n' then init tbef else tbef)

gherkin :: GenParser st Gherkin
gherkin
  = manyTill anyChar (try $ newline >> newline)
    >> rol
    >> (concat <$> many steps)

steps :: GenParser st [(Step, Text)]
steps = choice $ map step [minBound .. maxBound]

step :: Step -> GenParser st [(Step, Text)]
step name
  = try
    $ do
      spaces
      void $ string $ show name
      spaces
      text <- rol
      rest <- many $ try $ spaces >> string "And" >> spaces >> rol
      return $ map ((,) name) $ strip text : rest

rol :: GenParser st Text
rol = pack <$> manyTill anyChar (void newline <|> eof)
