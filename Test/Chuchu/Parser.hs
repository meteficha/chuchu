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

module Test.Chuchu.Parser (chuchu, st, number, module Test.Chuchu.Types) where

-- base
import Control.Applicative hiding ((<|>))
import Control.Monad

-- parsec
import Text.Parsec

-- chuchu
import Test.Chuchu.Types
import Test.Chuchu.Parsec

chuchu :: Monad m => [Chuchu m] -> Chuchu m
chuchu = choice . map try

st :: Monad m => String -> Chuchu m
st = void . string

number :: Monad m => ChuchuM m Double
number = nofToDouble <$> natFloat

nofToDouble :: Either Integer Double -> Double
nofToDouble (Left i) = fromInteger i
nofToDouble (Right d) = d
