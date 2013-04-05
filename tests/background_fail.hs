{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.IORef
import System.Environment
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import Test.Chuchu
import qualified Control.Exception as E


data Result = NothingExecd | JustBackground | AllSteps

type BackgroundFail = StateT (IORef Result) IO


defs :: Chuchu BackgroundFail
defs = do
  let reached s = do
        v <- get
        liftIO (writeIORef v s)

  Given "that this step will fail" $ \() -> do
    reached JustBackground
    fail "..."

  Then "this step should not be executed" $ \() ->
    reached AllSteps

main :: IO ()
main =
  withArgs ["tests/data/background_fail.feature"] $ do
    v <- newIORef NothingExecd
    E.catch (chuchuMain defs (`evalStateT` v))
            (\exitExc -> let _ = exitExc `asTypeOf` ExitSuccess
                         in return ())
    final <- readIORef v
    case final of
      NothingExecd   -> exitWith (ExitFailure 40)
      JustBackground -> exitWith ExitSuccess
      AllSteps       -> exitWith (ExitFailure 80)
