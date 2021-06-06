-- safediv is a function which  divides two number and handles invalid input
safediv :: Int -> Int -> Maybe Int
safediv n m = if m == 0 then 
                Nothing
              else
                Just (div n m)

-- safefact is a function which find factorial of a number and handles invalid input
safefact :: Integer -> Maybe Integer
safefact n = if n < 0 then
              Nothing
             else             
               Just (go n 1)

go :: Integer -> Integer -> Integer
go 0 x = x
go 1 x = x
go n x = go (n-1) (n * x)
