{-# OPTIONS_GHC -w #-}
-- |
-- Module      :  Test.Chuchu.Parsec
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
--
-- This is from the where clause of 'makeTokenParser' with types included.
module Test.Chuchu.Parsec (natFloat, int, module Test.Chuchu.Types) where

-- base
import Data.Char

-- parsec
import Text.Parsec
import Text.Parsec.Text ()

-- chuchu
import Test.Chuchu.Types

{-# ANN module "HLint: ignore" #-}
natFloat :: ChuchuM (Either Integer Double)
natFloat        = do{ char '0'
                    ; zeroNumFloat
                    }
                  <|> decimalFloat

zeroNumFloat :: ChuchuM (Either Integer Double)
zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                     ; return (Left n)
                     }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)

decimalFloat :: ChuchuM (Either Integer Double)
decimalFloat    = do{ n <- decimal
                    ; option (Left n)
                             (fractFloat n)
                    }

fractFloat :: Integer -> ChuchuM (Either Integer Double)
fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }

fractExponent :: Integer -> ChuchuM Double
fractExponent n = do{ fract <- fraction
                    ; expo  <- option 1.0 exponent'
                    ; return ((fromInteger n + fract)*expo)
                    }
                <|>
                  do{ expo <- exponent'
                    ; return ((fromInteger n)*expo)
                    }
fraction :: ChuchuM Double
fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return (foldr op 0.0 digits)
                    }
                  <?> "fraction"
                where
                  op d f    = (f + fromIntegral (digitToInt d))/10.0

exponent' :: ChuchuM Double
exponent'       = do{ oneOf "eE"
                    ; f <- sign
                    ; e <- decimal <?> "exponent"
                    ; return (power (f e))
                    }
                  <?> "exponent"
                where
                   power e  | e < 0      = 1.0/power(-e)
                            | otherwise  = fromInteger (10^e)

-- | This is the only function of this module that have been changed.  The call
-- to 'lexeme' before 'sign' has been removed.
int :: ChuchuM Integer
int             = do{ f <- sign
                    ; n <- nat
                    ; return (f n)
                    }

sign :: Num a => ChuchuM (a -> a)
sign            =   (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id

nat :: ChuchuM Integer
nat             = zeroNumber <|> decimal

zeroNumber :: ChuchuM Integer
zeroNumber      = do{ char '0'
                    ; hexadecimal <|> octal <|> decimal <|> return 0
                    }
                  <?> ""

decimal :: ChuchuM Integer
decimal         = number_ 10 digit

hexadecimal :: ChuchuM Integer
hexadecimal     = do{ oneOf "xX"; number_ 16 hexDigit }

octal :: ChuchuM Integer
octal           = do{ oneOf "oO"; number_ 8 octDigit  }

number_ :: Integer -> ChuchuM Char -> ChuchuM Integer
number_ base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }
