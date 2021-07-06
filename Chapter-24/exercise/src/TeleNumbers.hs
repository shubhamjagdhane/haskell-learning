module TeleNumbers where

import Text.Trifecta

type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone =  do 
  numberingPlanArea <- integer
  _ <- char '-'
  exchange <- integer
  _ <- char '-'
  PhoneNumber (fromInteger numberingPlanArea) (fromInteger exchange). fromInteger <$> integer
