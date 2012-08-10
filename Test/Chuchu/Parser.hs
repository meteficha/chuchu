{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      :  Test.Chuchu.Parser
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (TypeSynonymInstances, FlexibleInstances)
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

number :: Parser Double
number = nofToDouble <$> P.natFloat

nofToDouble :: Either Integer Double -> Double
nofToDouble (Left i) = fromInteger i
nofToDouble (Right d) = d

int :: Parser Integer
int = P.int

text :: Parser String
text = P.stringLiteral

wildcard :: String -> Parser ()
wildcard = void . manyTill anyChar . string
