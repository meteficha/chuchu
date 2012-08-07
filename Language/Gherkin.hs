module
  Language.Gherkin
  (parseFile, parseGherkin, Gherkin (..), Scenario (..), Step (..))
  where

-- base
import Prelude hiding (readFile, unlines, last, init, lines)
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

data Gherkin
  = Gherkin
      {gTags :: [Text],
        gName :: Text,
        gDescription :: [Text],
        gScenarios :: [Scenario]}
    deriving (Eq, Show)

data Scenario
  = Scenario {sName :: Text, sSteps :: [(Step, Text)]}
    deriving (Eq, Show)

data Step = Given | When | Then deriving (Eq, Bounded, Enum, Show)

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
  = do
    tags <- many $ try tag
    void $ string "Feature: "
    name <- rol
    description <- manyTill anyChar (try $ newline >> newline)
    scenarios <- many $ scenario
    return
      $ Gherkin
        tags
        (strip name)
        (map strip $ lines $ pack description)
        scenarios

tag :: GenParser st Text
tag
  = do
    void $ char '@'
    str <- many $ letter <|> char '_'
    void newline
    return $ strip $ pack str

scenario :: GenParser st Scenario
scenario
  = do
    spaces
    void $ string "Scenario: "
    name <- rol
    steps <- many step
    return $ Scenario (strip name) $ concat steps

step :: GenParser st [(Step, Text)]
step = choice $ map stepF [minBound .. maxBound]

stepF :: Step -> GenParser st [(Step, Text)]
stepF name
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
