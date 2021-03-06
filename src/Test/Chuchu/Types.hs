-- |
-- Module      :  Test.Chuchu.Types
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (GADTs, GeneralizedNewtypeDeriving)
module
  Test.Chuchu.Types
  (ChuchuParser (..), Chuchu, ChuchuM (Given, When, Then, And, But), runChuchu)
  where

import Control.Applicative (Applicative((<*)), Alternative((<|>)), (<$>))
import Control.Monad (MonadPlus)
import Data.String (IsString(..))
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P (Parser)


-- | @newtype@ for Parsec's 'Parser' used on this library.  The
-- main reason for not using 'Parser' directly is to be able to
-- define the 'IsString' instance.
newtype ChuchuParser a = ChuchuParser (P.Parser a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance (a ~ ()) => IsString (ChuchuParser a) where
  fromString s = ChuchuParser (P.try (P.string s) >> return ())


-- | The most common use case where the return value of the Monad is ignored.
type Chuchu m  = ChuchuM m ()

-- | The Monad on which the step rules are constructed.  'Given', 'When',
-- 'Then', 'And' and 'But' are interpreted in the same way by the program.  All
-- of them receive a parser and an action to run if the parser finishes
-- correctly.
data ChuchuM m a where
  Given :: ChuchuParser a -> (a -> m ()) -> ChuchuM m ()
  When :: ChuchuParser a -> (a -> m ()) -> ChuchuM m ()
  Then :: ChuchuParser a -> (a -> m ()) -> ChuchuM m ()
  And :: ChuchuParser a -> (a -> m ()) -> ChuchuM m ()
  But :: ChuchuParser a -> (a -> m ()) -> ChuchuM m ()
  Nil :: ChuchuM m a
  Cons :: ChuchuM m b -> ChuchuM m a -> ChuchuM m a

instance Monad (ChuchuM m) where
  return _ = Nil
  step >>= k = Cons step $ k $ error "(>>=): ChuchuM does not support 'return'."

-- | Converts the Monad into a single 'Parser' that executes the specified
-- action if the parser is executed correctly.  It includes an 'eof' on the
-- parser of each step to avoid it from accepting prefixes of the desired rule.
runChuchu :: ChuchuM m a -> P.Parser (m ())
runChuchu Nil = P.unexpected "Unknown step"
runChuchu (Cons cc1 cc2) = runChuchu cc1 <|> runChuchu cc2
runChuchu (Given p f) = apply p f
runChuchu (When p f) = apply p f
runChuchu (Then p f) = apply p f
runChuchu (And p f) = apply p f
runChuchu (But p f) = apply p f

apply :: ChuchuParser a -> (a -> m ()) -> P.Parser (m ())
apply (ChuchuParser p) f = P.try $ f <$> p <* P.eof
