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
module Test.Chuchu.Parser (number, int, text, wildcard) where

-- base
import Control.Applicative hiding ((<|>))
import Control.Monad
import GHC.Exts

-- parsec
import Text.Parsec
import Text.Parsec.Text

-- chuchu
import qualified Test.Chuchu.Parsec as P

instance IsString (Parser a) where
  fromString s = void (string s) >> return undefined

-- | Parses a floating point number, with the same syntax as accepted by
-- Haskell.
number :: Parser Double
number = nofToDouble <$> P.natFloat

nofToDouble :: Either Integer Double -> Double
nofToDouble (Left i) = fromInteger i
nofToDouble (Right d) = d

-- | Parses an integer.
int :: Parser Integer
int = P.int

-- | Parses a quoted string, with the same syntax as accepted by Haskell.
text :: Parser String
text = P.stringLiteral

-- | Parses anything until the string passed as parameter, and also the string.
wildcard :: String -> Parser ()
wildcard = void . manyTill anyChar . string
