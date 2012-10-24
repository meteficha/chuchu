-- |
-- Module      :  Test.Chuchu.Parsec
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
--
-- This is a very simplified parser for e-mail, which does not follow RFC5322.
-- Basically, it parses @TEXT\@TEXT@, where TEXT is @alphaNum <|> oneOf
-- "!#$%&'*+-/=?^_`{|}~."@.  It's loosely based on
-- <http://porg.es/blog/email-address-validation-simpler-faster-more-correct>.
module Test.Chuchu.Email (addrSpecSimple) where

-- base
import Control.Applicative hiding ((<|>), many, optional)

-- parsec
import Text.Parsec
import Text.Parsec.Text

-- | Parses a simplified e-mail address and return everything that was parsed as
-- a simple 'String'.
addrSpecSimple :: Parser String
addrSpecSimple = concat <$> sequence [atomWithDot, string "@", atomWithDot]

atomWithDot :: Parser String
atomWithDot = many1 atomTextWithDot

atomTextWithDot :: Parser Char
atomTextWithDot = alphaNum <|> oneOf "!#$%&'*+-/=?^_`{|}~."
