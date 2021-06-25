module EitherMonda where

type Founded = Int 
type Coders  = Int 

data SoftwareShop = 
  Shop {
      founded :: Founded
    , programmers  :: Coders
  } deriving (Show, Eq)


data FoundedError = 
    NegativeYears        Founded
  | TooManyYears         Founded
  | NegativeCoders       Coders
  | TooManyCoders        Coders
  | TooManyCoderForYears Founded Coders
  deriving (Show, Eq)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n 
  | n < 0      = Left $ NegativeYears n
  | n > 500    = Left $ TooManyYears n
  | otherwise  = Right n 

validateCoder :: Int -> Either FoundedError Coders
validateCoder n 
  | n < 0      = Left $ NegativeCoders n
  | n > 5000   = Left $ TooManyYears n
  | otherwise  = Right n   

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoder coders
  if programmers > div founded 10
    then Left $ TooManyCoderForYears founded programmers
    else Right $ Shop founded programmers
