-- |
-- Module      :  Test.Chuchu.Types
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (GADTs)
module
  Test.Chuchu.Types (Chuchu, ChuchuM (..), runChuchu, Parser)
  where

-- base
import Control.Applicative hiding ((<|>))

-- parsec
import Text.Parsec
import Text.Parsec.Text

type Chuchu m  = ChuchuM m ()

data ChuchuM m a where
  Given :: Parser a -> (a -> m ()) -> ChuchuM m ()
  When :: Parser a -> (a -> m ()) -> ChuchuM m ()
  Then :: Parser a -> (a -> m ()) -> ChuchuM m ()
  And :: Parser a -> (a -> m ()) -> ChuchuM m ()
  But :: Parser a -> (a -> m ()) -> ChuchuM m ()
  Nil :: ChuchuM m a
  Cons :: ChuchuM m b -> ChuchuM m a -> ChuchuM m a

instance Monad (ChuchuM m) where
  return _ = Nil
  step >>= k = Cons step $ k undefined

runChuchu :: ChuchuM m a -> Parser (m ())
runChuchu Nil = unexpected "Unknown step"
runChuchu (Cons cc1 cc2) = runChuchu cc1 <|> runChuchu cc2
runChuchu (Given p f) = apply p f
runChuchu (When p f) = apply p f
runChuchu (Then p f) = apply p f
runChuchu (And p f) = apply p f
runChuchu (But p f) = apply p f

apply :: Parser a -> (a -> m ()) -> Parser (m ())
apply p f = try $ f <$> p <* eof
