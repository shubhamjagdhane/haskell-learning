module Chapter5 where

addStuff :: Integer -> (Integer->Integer)
addStuff = \a -> \b -> a + b


-- Uncurried functions: One function, many arguments
-- Curried functions: Many functions, one argument apiece

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

typicalCurriedFunction :: Integer -> Bool -> Integer
typicalCurriedFunction i b = i + (nonsense b)

uncurriedFunction :: (Integer, Bool) -> Integer 
uncurriedFunction (i, b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousAndManuallyNested :: Integer -> Bool -> Integer
anonymousAndManuallyNested = \i -> \b -> i + (nonsense b)


-- Broadly speaking, type signatures may have three kinds of types: concrete, constrained polymorphic, or parametrically polymorphic.

-- A subclass cannot override the methods of its superclass

a = 6 / fromIntegral (length [1, 2, 3])
