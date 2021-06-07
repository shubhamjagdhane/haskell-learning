module Arith4 where

roundTrip :: (Read a, Show a) => a -> a
--roundTrip a = read (show a)

-- pointfree of roundTrip

roundTrip = \a -> read . show $a

main = do
  print (roundTrip 4)
  print (id 4)
