-- |
-- Module      :  Test.Chuchu.Types
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
module Test.Chuchu.Types (Chuchu, ChuchuM) where

-- parsec
import Text.Parsec.Text

type Chuchu m = ChuchuM (m ())
type ChuchuM = Parser
