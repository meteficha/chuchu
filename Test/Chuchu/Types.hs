-- |
-- Module      :  Test.Chuchu.Types
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
module Test.Chuchu.Types (Chuchu, ChuchuM) where

-- text
import Data.Text

-- parsec
import Text.Parsec

type Chuchu m = ChuchuM m ()
type ChuchuM m a = ParsecT Text () m a

