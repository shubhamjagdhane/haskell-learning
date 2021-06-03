module Changemood where
data Mood = Blah | Woot deriving Show


changeMood Woot = Blah
changeMood Blah = Woot


data Name = First | Second deriving Show

getName First = Second
getName Second = First

-- some type contructor take agruments
-- type constructor Bool
data Mybool = False | True

-- if else statement
greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
  then putStrLn "eyyyyy. What's shakin'?"
  else
  putStrLn "pshhhh."
  where cool = coolness == "downright frosty yo"

-- not yet understand properly
type PetName = String
data Pet = Cat | Dog PetName

