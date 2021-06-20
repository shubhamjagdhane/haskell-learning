-- functors

-- a functor is a TYPECLASS

-- as there are wrapped value and unwrapped value
-- Just x where x is wrapped with Just
-- y, y is unwrapped
-- to work on wrapped value functor plays a role

{-

I/O = [(+2), (+3)] <*> [1..5]
O/P = [3,4,5,6,7,4,5,6,7,8]

-}

data Maybe2 a = Just2 a | Nothing2 deriving Show 

instance Functor Maybe2 where
  fmap f (Just2 x) = Just2 $ f x 
  fmap f Nothing2  = Nothing2

instance Applicative Maybe2 where
  pure           = Just2
  Just2 f <*> j  = fmap f j
  Nothing2 <*> j = Nothing2

-- Monad needs Applicative and Functor instance
instance Monad Maybe2 where
  Nothing2 >>= f  = Nothing2
  Just2 val >>= f = f val



data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving Show

instance Functor BinaryTree where
  fmap func Leaf                    = Leaf
  fmap func (Node left value right) = Node (fmap func left) (func value) (fmap func right)


{-

x = Node Leaf 10 (Node Leaf 20 (Node Leaf 30 Leaf))
y = (+5) <$> x

y will be Node Leaf 15 (Node Leaf 25 (Node Leaf 35 Leaf))


-}

-- applicatives

{-

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

if both of the parameters are of wrapped value then we used <*>
e.g. (Just (*3)) <*> Just 10 => Just 30
-}

data Tree a = Tip a | Branch (Tree a) (Tree a) deriving Show

instance Functor Tree where
  fmap f (Tip x) = Tip $ f x
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative Tree where
  pure = Tip
  Tip f <*> g = fmap f g
  Branch left right <*> g = Branch (left <*> g) (right <*> g)

instance Monad Tree where
  Tip x >>= f = f x
  Branch left right >>= f = Branch (left >>= f) (right >>= f)

{-
example 1:
x = Branch (Tip 3) (Branch (Tip 5) (Tip 6))
y = Tip (*10) <*> x

we can see that argument to <*> have wrapped values

y will be Branch (Tip 30) (Branch (Tip 50) (Tip 60))

example 2:
x = Branch (Tip 3) (Branch (Tip 5) (Tip 6)) 
I/P = Branch (Tip (+1)) (Tip (+2)) <*> x
O/P = Branch (Branch (Tip 4) (Branch (Tip 6) (Tip 7))) (Branch (Tip 5) (Branch (Tip 7) (Tip 8)))

-}

-- Monoid
{-

Monoid applies a function (regular function (+3) (+2)) to a wrapped value and returns a wrapped value

-}


half :: (Integral a) => a -> Maybe2 a
half x = if even x then Just2 $ div x 2 else Nothing2


{-

I/P: Just2 10 >>= half
O/P: Just2 5
I/P: Just2 16 >>= half >>= half 
O/P: Just2 4

-}

g :: Integer -> Tree Integer
g x | x == 4 = Tip 99 | otherwise = Branch (Tip (x*2)) (Tip (x*4))

{-

x = Branch (Tip 3) (Branch (Tip 5) (Tip 6))

x >>= g
Branch (Branch (Tip 6) (Tip 12)) (Branch (Branch (Tip 10) (Tip 20)) (Branch (Tip 12) (Tip 24)))


-}
