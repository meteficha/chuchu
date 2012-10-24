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
  ) where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Text.Parsec
import qualified Data.Text as T

import Test.Chuchu.Types (ChuchuParser(..))
import Test.Chuchu.Email
import qualified Test.Chuchu.Parsec as P

-- | Parses a floating point number, with the same syntax as accepted by
-- Haskell.
number :: ChuchuParser Double
number = ChuchuParser $ nofToDouble <$> P.natFloat

nofToDouble :: Either Integer Double -> Double
nofToDouble (Left i) = fromInteger i
nofToDouble (Right d) = d

-- | Parses an integer.
int :: ChuchuParser Integer
int = ChuchuParser P.int

-- | Parses a quoted string, with the same syntax as accepted by Haskell.
text :: ChuchuParser T.Text
text = T.pack <$> ChuchuParser P.stringLiteral

-- | Parses anything until the string passed as parameter, and also the string.
wildcard :: T.Text -> ChuchuParser ()
wildcard = ChuchuParser . void . manyTill anyChar . string . T.unpack

-- | Parses a simplified e-mail address and return everything that was parsed as
-- a simple 'T.Text'.  This is a very simplified parser for e-mail, which does
-- not follow RFC5322. Basically, it parses @TEXT\@TEXT@, where TEXT is
-- @alphaNum \<|> oneOf \"!#$%&\'*+-\/=?^_\`{|}~.\"@.
email :: ChuchuParser T.Text
email = T.pack <$> ChuchuParser addrSpecSimple