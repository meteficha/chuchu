-- |
-- Module      :  Test.Chuchu.Parser
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
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

st :: String -> ChuchuM ()
st = void . string

number :: ChuchuM Double
number = nofToDouble <$> natFloat

nofToDouble :: Either Integer Double -> Double
nofToDouble (Left i) = fromInteger i
nofToDouble (Right d) = d
