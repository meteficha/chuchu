-- Copyright 2012 Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- base
import Control.Applicative
import Data.Maybe

-- unix
import System.Posix.Env

-- chuchu
import Test.Chuchu

-- HUnit
import Test.HUnit

enterNumber :: Int -> IO ()
enterNumber n
  = do
    putStrLn "setting..."
    setEnv "environment" (show n) True

getNumber :: IO Int
getNumber
  = do
    putStrLn "getting..."
    read <$> fromJust <$> getEnv "environment"

defs :: Chuchu IO
defs = [
  ([CPT "I set the variable as ", Number, CPT " into the environment"],
    \ [n] -> enterNumber n),
  ([CPT "the variable should have ", Number, CPT " on its content"],
    \ [n]
      -> do
        putStrLn "getting...1"
        d <- getNumber
        n @=? d)]

main :: IO ()
main = chuchuMain defs id "tests/data/environment.feature"
