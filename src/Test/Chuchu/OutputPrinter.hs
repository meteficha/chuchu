module Test.Chuchu.OutputPrinter
  ( putDoc
  , warn
  , describeAbacate
  , describeBasicScenario
  , BasicScenarioKind(..)
  , describeStep
  , StepResult(..)
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Language.Abacate hiding (StepKeyword (..))
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as D


-- | Print a 'D.Doc' describing what we're currently processing.
putDoc :: MonadIO m => D.Doc -> m ()
putDoc = liftIO . D.putDoc . (D.<> D.linebreak)


-- | Print a warning message.
warn :: MonadIO m => String -> m ()
warn = liftIO . hPutStrLn stderr


----------------------------------------------------------------------


-- | Same as 'D.text' but using 'T.Text'.
t2d :: T.Text -> D.Doc
t2d = D.text . T.unpack


-- | Creates a pretty description of the feature.
describeAbacate :: Abacate -> D.Doc
describeAbacate feature =
  D.vsep $
  describeTags (fTags feature) ++ [D.white $ t2d $ fHeader feature]


-- | Creates a vertical list of tags.
describeTags :: Tags -> [D.Doc]
describeTags = map (D.dullcyan . ("@" D.<>) . t2d)


-- | Creates a pretty description of the basic scenario's header.
describeBasicScenario :: BasicScenarioKind -> BasicScenario -> D.Doc
describeBasicScenario kind scenario =
  D.indent 2 $
  prettyTags kind $
  D.bold ((describeBasicScenarioKind kind) D.<+> t2d (bsName scenario))
    where describeBasicScenarioKind BackgroundKind   = "Background:"
          describeBasicScenarioKind (ScenarioKind _) = "Scenario:"

          prettyTags BackgroundKind      = id
          prettyTags (ScenarioKind tags) = D.vsep . (describeTags tags ++) . (:[])


data BasicScenarioKind = BackgroundKind | ScenarioKind Tags


-- | Pretty-prints a step that has already finished executing.
describeStep :: StepResult -> Step -> D.Doc
describeStep result step =
  D.indent 4 $
  D.vsep $ [color result (D.text (show $ stStepKeyword step) D.<+> t2d (stBody step))]
        ++ map D.text (errMsg result)
    where
      color SuccessfulStep  = D.green
      color (FailedStep _)  = D.red
      color (UnknownStep _) = D.yellow
      errMsg SuccessfulStep  = []
      errMsg (FailedStep m)  = [m]
      errMsg (UnknownStep m) = [m]

data StepResult = SuccessfulStep
                | FailedStep String
                | UnknownStep String
                  deriving (Eq)
