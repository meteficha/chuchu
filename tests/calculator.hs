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
import System.Environment

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.State

-- chuchu
import Test.Chuchu

-- HUnit
import Test.HUnit

type CalculatorT m = StateT [Double] m

enterNumber :: Monad m => Double -> CalculatorT m ()
enterNumber = modify . (:)

getDisplay :: Monad m => CalculatorT m Double
getDisplay
  = do
    ns <- get
    return $ head $ ns ++ [0]

divide :: Monad m => CalculatorT m ()
divide = do
  (n1:n2:ns) <- get
  put $ (n1 / n2) : ns

defs :: Chuchu (CalculatorT IO)
defs
  = chuchu
    [enterNumber
        <$ st "that I have entered "
        <*> number
        <* st " into the calculator",
      divide <$ st "I press divide",
      let
          act n
            = do
              d <- getDisplay
              liftIO $ d @?= n
        in act <$ st "the result should be " <*> number <* st " on the screen"]

main :: IO ()
main
  = withArgs ["tests/data/calculator.feature"]
    $ chuchuMain defs (`evalStateT` [])
