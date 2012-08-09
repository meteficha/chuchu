-- Copyright 2012 Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# OPTIONS_GHC -w #-}
-- | This is from the where clause of makeTokenParser with types included.
module Test.Chuchu.Parsec (natFloat, module Test.Chuchu.Types) where

-- base
import Data.Char

-- parsec
import Text.Parsec
import Text.Parsec.Text ()

-- chuchu
import Test.Chuchu.Types

natFloat :: Monad m => ChuchuM m (Either Integer Double)
natFloat        = do{ char '0'
                    ; zeroNumFloat
                    }
                  <|> decimalFloat

zeroNumFloat :: Monad m => ChuchuM m (Either Integer Double)
zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                     ; return (Left n)
                     }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)

decimalFloat :: Monad m => ChuchuM m (Either Integer Double)
decimalFloat    = do{ n <- decimal
                    ; option (Left n)
                             (fractFloat n)
                    }

fractFloat :: Monad m => Integer -> ChuchuM m (Either Integer Double)
fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }

fractExponent :: Monad m => Integer -> ChuchuM m Double
fractExponent n = do{ fract <- fraction
                    ; expo  <- option 1.0 exponent'
                    ; return ((fromInteger n + fract)*expo)
                    }
                <|>
                  do{ expo <- exponent'
                    ; return ((fromInteger n)*expo)
                    }
fraction :: Monad m => ChuchuM m Double
fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return (foldr op 0.0 digits)
                    }
                  <?> "fraction"
                where
                  op d f    = (f + fromIntegral (digitToInt d))/10.0

exponent' :: Monad m => ChuchuM m Double
exponent'       = do{ oneOf "eE"
                    ; f <- sign
                    ; e <- decimal <?> "exponent"
                    ; return (power (f e))
                    }
                  <?> "exponent"
                where
                   power e  | e < 0      = 1.0/power(-e)
                            | otherwise  = fromInteger (10^e)

sign :: (Monad m, Num a) => ChuchuM m (a -> a)
sign            =   (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id

decimal :: Monad m => ChuchuM m Integer
decimal         = number_ 10 digit

hexadecimal :: Monad m => ChuchuM m Integer
hexadecimal     = do{ oneOf "xX"; number_ 16 hexDigit }

octal :: Monad m => ChuchuM m Integer
octal           = do{ oneOf "oO"; number_ 8 octDigit  }

number_ :: Monad m => Integer -> ChuchuM m Char -> ChuchuM m Integer
number_ base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }
