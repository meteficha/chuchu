{-# LANGUAGE OverloadedStrings #-}


import Control.Monad.Trans.State
import System.Environment
import Test.Chuchu


type Bathroom = StateT Door IO


data Door = Open | Locked


defs :: Chuchu Bathroom
defs = do

  Given "that Bob entered the bathroom and locked the door" $ \_ -> do
         door <- get
         case door of
           Open -> put Locked
           Locked -> fail "Door was already locked."

  Given "that Alice entered the bathroom and locked the door" $ \_ -> do
         door <- get
         case door of
           Open -> put Locked
           Locked -> fail "Door was already locked."

main :: IO ()
main = withArgs ["tests/data/multiple_scenarios.feature"] $
       chuchuMain defs (`evalStateT` Open)
