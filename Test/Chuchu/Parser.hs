-- |
-- Module      :  Test.Chuchu.Parser
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
module
  Test.Chuchu.Parser
  (chuchu, st, number, int, text, module Test.Chuchu.Types)
  where

-- base
import Control.Applicative hiding ((<|>))
import Control.Monad

-- parsec
import Text.Parsec

-- chuchu
import Test.Chuchu.Types
import qualified Test.Chuchu.Parsec as P

chuchu :: Monad m => [Chuchu m] -> Chuchu m
chuchu = choice . map (try . (<* eof))

st :: String -> ChuchuM ()
st = void . string

number :: ChuchuM Double
number = nofToDouble <$> P.natFloat

nofToDouble :: Either Integer Double -> Double
nofToDouble (Left i) = fromInteger i
nofToDouble (Right d) = d

int :: ChuchuM Integer
int = P.int

text :: ChuchuM String
text = P.stringLiteral
