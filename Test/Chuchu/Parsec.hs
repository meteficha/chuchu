{-# OPTIONS_GHC -w #-}
-- Copyright 1999-2000, Daan Leijen; 2007, Paolo Martini. All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.

-- This software is provided by the copyright holders "as is" and any express or
-- implied warranties, including, but not limited to, the implied warranties of
-- merchantability and fitness for a particular purpose are disclaimed. In no
-- event shall the copyright holders be liable for any direct, indirect,
-- incidental, special, exemplary, or consequential damages (including, but not
-- limited to, procurement of substitute goods or services; loss of use, data,
-- or profits; or business interruption) however caused and on any theory of
-- liability, whether in contract, strict liability, or tort (including
-- negligence or otherwise) arising in any way out of the use of this software,
-- even if advised of the possibility of such damage.

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
module Test.Chuchu.Parsec (stringLiteral, natFloat, int) where

-- base
import Data.Char

-- parsec
import Text.Parsec
import Text.Parsec.Text

{-# ANN module ("HLint: ignore" :: String) #-}
-- | 'lexeme' removed.
stringLiteral :: Parser String
stringLiteral   = do{ str <- between (char '"')
                                     (char '"' <?> "end of string")
                                     (many stringChar)
                    ; return (foldr (maybe id (:)) "" str)
                    }
                  <?> "literal string"

stringChar :: Parser (Maybe Char)
stringChar      =   do{ c <- stringLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"


stringLetter :: Parser Char
stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape :: Parser (Maybe Char)
stringEscape    = do{ char '\\'
                    ;     do{ escapeGap  ; return Nothing }
                      <|> do{ escapeEmpty; return Nothing }
                      <|> do{ esc <- escapeCode; return (Just esc) }
                    }

escapeEmpty :: Parser Char
escapeEmpty     = char '&'


escapeGap :: Parser Char
escapeGap       = do{ many1 space
                    ; char '\\' <?> "end of string gap"
                    }

escapeCode :: Parser Char
escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                <?> "escape code"

charControl :: Parser Char
charControl     = do{ char '^'
                    ; code <- upper
                    ; return (toEnum (fromEnum code - fromEnum 'A'))
                    }

charNum :: Parser Char
charNum         = do{ code <- decimal
                              <|> do{ char 'o'; number 8 octDigit }
                              <|> do{ char 'x'; number 16 hexDigit }
                    ; return (toEnum (fromInteger code))
                    }

charEsc :: Parser Char
charEsc         = choice (map parseEsc escMap)
                where
                  parseEsc (c,code)     = do{ char c; return code }

charAscii :: Parser Char
charAscii       = choice (map parseAscii asciiMap)
                where
                  parseAscii (asc,code) = try (do{ string asc; return code })


-- escape code tables
escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                   "FS","GS","RS","US","SP"]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                   "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                   "CAN","SUB","ESC","DEL"]

ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                   '\EM','\FS','\GS','\RS','\US','\SP']
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                   '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                   '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


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
