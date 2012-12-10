{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      :  Test.Chuchu.Parser
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (TypeSynonymInstances, FlexibleInstances)
--
-- This modules provides some utility parsers for creating step rules.
module Test.Chuchu.Parser
  ( ChuchuParser
  , number
  , int
  , text
  , wildcard
  , email
  , try
  ) where

import Control.Applicative ((<$>))
import qualified Data.Text as T
import qualified Text.Parsec as P

import Test.Chuchu.Types (ChuchuParser(..))
import Test.Chuchu.Email
import qualified Test.Chuchu.Parsec as P'


-- | Parses a floating point number, with the same syntax as accepted by
-- Haskell.
number :: ChuchuParser Double
number = ChuchuParser $ nofToDouble <$> P'.natFloat


nofToDouble :: Either Integer Double -> Double
nofToDouble (Left i) = fromInteger i
nofToDouble (Right d) = d


-- | Parses an integer.
int :: ChuchuParser Integer
int = ChuchuParser P'.int


-- | Parses a quoted string, with the same syntax as accepted by Haskell.
text :: ChuchuParser T.Text
text = T.pack <$> ChuchuParser P'.stringLiteral


-- | Parses anything until the string passed as parameter, and also the string.
wildcard :: T.Text -> ChuchuParser T.Text
wildcard = fmap T.pack . ChuchuParser . P.manyTill P.anyChar . P.try . P.string . T.unpack


-- | Parses a simplified e-mail address and return everything that was parsed as
-- a simple 'T.Text'.  This is a very simplified parser for e-mail, which does
-- not follow RFC5322. Basically, it parses @TEXT\@TEXT@, where TEXT is
-- @alphaNum \<|> oneOf \"!#$%&\'*+-\/=?^_\`{|}~.\"@.
email :: ChuchuParser T.Text
email = T.pack <$> ChuchuParser addrSpecSimple


-- | Same as Parsec's 'Parsec.try' but for 'ChuchuParser'.
try :: ChuchuParser a -> ChuchuParser a
try (ChuchuParser x) = ChuchuParser (P.try x)
