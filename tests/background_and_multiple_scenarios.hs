{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Trans.State
import System.Environment
import Test.Chuchu


type Bathroom = StateT Door IO


data Door = Open | Locked


defs :: Chuchu Bathroom
defs = do

  Given "that the bathroom's door is open" $ \_ ->
         put Open

  Given ("that " *> text <* " has entered the bathroom and locked the door") $ \_ -> do
    door <- get
    case door of
      Open -> put Locked
      Locked -> fail "Door was already locked."


main :: IO ()
main =  withArgs ["tests/data/background_and_multiple_scenarios.feature"] $
        chuchuMain defs (`evalStateT` Locked)
