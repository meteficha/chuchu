{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      :  Test.Chuchu.Parser
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (TypeSynonymInstances, FlexibleInstances)
module
  Test.Chuchu.Parser
  (chuchu, number, int, text, module Test.Chuchu.Types)
  where

-- base
import Control.Applicative hiding ((<|>))
import Control.Monad
import GHC.Exts

-- parsec
import Text.Parsec

-- chuchu
import Test.Chuchu.Types
import qualified Test.Chuchu.Parsec as P

instance IsString (ChuchuM a) where
  fromString s = void (string s) >> return undefined

chuchu :: Monad m => [Chuchu m] -> Chuchu m
chuchu = choice . map (try . (<* eof))

number :: ChuchuM Double
number = nofToDouble <$> P.natFloat

nofToDouble :: Either Integer Double -> Double
nofToDouble (Left i) = fromInteger i
nofToDouble (Right d) = d

int :: ChuchuM Integer
int = P.int

text :: ChuchuM String
text = P.stringLiteral
