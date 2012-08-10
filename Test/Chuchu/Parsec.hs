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
-- This is from the where clause of 'makeTokenParser' with types included and
-- calls to 'lexeme' removed in the functions where this is noted.
module Test.Chuchu.Parsec (natFloat, int) where

-- base
import Data.Char

-- parsec
import Text.Parsec
import Text.Parsec.Text

{-# ANN module "HLint: ignore" #-}
natFloat :: Parser (Either Integer Double)
natFloat        = do{ char '0'
                    ; zeroNumFloat
                    }
                  <|> decimalFloat

zeroNumFloat :: Parser (Either Integer Double)
zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                     ; return (Left n)
                     }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)

decimalFloat :: Parser (Either Integer Double)
decimalFloat    = do{ n <- decimal
                    ; option (Left n)
                             (fractFloat n)
                    }

fractFloat :: Integer -> Parser (Either Integer Double)
fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }

fractExponent :: Integer -> Parser Double
fractExponent n = do{ fract <- fraction
                    ; expo  <- option 1.0 exponent'
                    ; return ((fromInteger n + fract)*expo)
                    }
                <|>
                  do{ expo <- exponent'
                    ; return ((fromInteger n)*expo)
                    }
fraction :: Parser Double
fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return (foldr op 0.0 digits)
                    }
                  <?> "fraction"
                where
                  op d f    = (f + fromIntegral (digitToInt d))/10.0

exponent' :: Parser Double
exponent'       = do{ oneOf "eE"
                    ; f <- sign
                    ; e <- decimal <?> "exponent"
                    ; return (power (f e))
                    }
                  <?> "exponent"
                where
                   power e  | e < 0      = 1.0/power(-e)
                            | otherwise  = fromInteger (10^e)

-- | 'lexeme' removed.
int :: Parser Integer
int             = do{ f <- sign
                    ; n <- nat
                    ; return (f n)
                    }

sign :: Num a => Parser (a -> a)
sign            =   (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id

nat :: Parser Integer
nat             = zeroNumber <|> decimal

zeroNumber :: Parser Integer
zeroNumber      = do{ char '0'
                    ; hexadecimal <|> octal <|> decimal <|> return 0
                    }
                  <?> ""

decimal :: Parser Integer
decimal         = number 10 digit

hexadecimal :: Parser Integer
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }

octal :: Parser Integer
octal           = do{ oneOf "oO"; number 8 octDigit  }

number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }
