-- |
-- Module      :  Test.Chuchu.Types
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (GADTs)
module
  Test.Chuchu.Types
  (Chuchu, ChuchuM (Given, When, Then, And, But), runChuchu)
  where

-- base
import Control.Applicative hiding ((<|>))

-- parsec
import Text.Parsec
import Text.Parsec.Text

-- | The most command use case where the return value of the Monad is ignored.
type Chuchu m  = ChuchuM m ()

-- | The Monad on which the step rules are constructed.  'Given', 'When',
-- 'Then', 'And' and 'But' are interpreted in the same way by the program.  All
-- of them receive a parser and an action to run if the parser finishes
-- correctly.
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

-- | Converts the Monad into a single 'Parser' that executes the specified
-- action if the parser is executed correctly.  It includes an 'eof' on the
-- parser of each step to avoid it from accepting prefixes of the desired rule.
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
