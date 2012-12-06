import Control.Applicative
import Control.Monad.Trans.State
import System.Environment
import Test.Chuchu


type Bathroom = StateT Door IO


data Door = Open | Locked


defs :: Chuchu Bathroom
defs = do

  Given ("that " *> text <* " has entered the bathroom and locked the door") $ \_ -> do
    door <- get
    case door of
      Open -> put Locked
      Locked -> fail "Door was already locked."


main :: IO ()
main = withArgs [ "tests/data/multiple_features1.feature"
                , "tests/data/multiple_features2.feature" ] $
       chuchuMain defs (`evalStateT` Open)
