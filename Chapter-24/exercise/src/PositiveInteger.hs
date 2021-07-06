module PositiveInteger where

import Text.Trifecta

parseDigit :: Parser Char
parseDigit = digit

base10Integer :: Parser Integer
base10Integer = integer

base10Integer' :: Parser Integer
base10Integer' = integer 