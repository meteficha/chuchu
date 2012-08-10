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

type CalculatorT m = StateT Integer m

defs :: [Chuchu (CalculatorT IO)]
defs
  = [When ("I add " *> int) $ \x -> modify (+ x),
    When ((,) <$ "I add " <*> int <* " " <*> int <* " times")
      $ \(n, x) -> modify (+ n * x),
    When ("the result should be " *> int) $ \x -> get >>= liftIO . (@?= x)]

main :: IO ()
main
  = withArgs ["tests/data/prefix.feature"]
    $ chuchuMain defs (`evalStateT` 0)
