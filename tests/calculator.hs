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

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.State

-- abacate
import Language.Abacate

-- chuchu
import Test.Chuchu

-- HUnit
import Test.HUnit

type CalculatorT m = StateT [Int] m

enterNumber :: Monad m => Int -> CalculatorT m ()
enterNumber = modify . (:)

getDisplay :: Monad m => CalculatorT m Int
getDisplay
  = do
    ns <- get
    return $ head $ ns ++ [0]

divide :: Monad m => CalculatorT m ()
divide = do
  (n1:n2:ns) <- get
  put $ (n1 `div` n2) : ns

defs :: Chuchu (CalculatorT IO)
defs = [
  (Given,
    [CPT "that I have entered ", Number, CPT " into the calculator"],
    \ [n] -> enterNumber n),
  (When, [CPT "I press divide"], const divide),
  (Then,
    [CPT "the result should be ", Number, CPT " on the screen"],
    \ [n]
      -> do
        d <- getDisplay
        liftIO $ d @?= n)]

main :: IO ()
main = chuchuMain defs (flip evalStateT []) "tests/data/calculator.feature"
