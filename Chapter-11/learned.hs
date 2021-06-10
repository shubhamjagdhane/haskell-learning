{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Type constructors are used only at the type level, in type signatures and typeclass declarations and instances.

-- Kinds are the types of types, or types one level up

data Doggies a =
  Husky a
  | Mastiff a
  deriving (Eq, Show)


-- types are static and resolve at compile time  
-- Data are what we’re working with at runtime
-- type Constructor = data Contructor
-- when data constructors take arguments, those arguments refer to other types

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = AirIndia | Emirates | Lufthansa deriving (Eq, Show) 


data Vehicle = Car Manufacturer Price |
               Plane Airline
               deriving (Eq, Show)

-- from the above Vehicle datatype
-- Vehicle is called as type constructor
-- Car, Plane are called as data constructor
-- Manufacturer, Price and Airline are called as type argument


-- Arity refers to the number of arguments a function or contructor takes

-- A function that takes no arguments is called nullary, where nullary is a contraction of “null” and “-ary”.
-- so do nullary, unary, binary so on

-- data constructors that take more than one argument are called products

-- The difference between newtype and the type it contains is gone by the time the compiler generates the code


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where 
  tooMany n = n > 42


-- the newtype declaration will allow you to define a custom instance
newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- instance classname type/data name where
{-
instance TooMany Goats where
  tooMany (Goats n) = tooMany n
-}


-- records

data Person = Person { name :: String
                     , age  :: Int }
                     deriving (Eq, Show)

-- function type is exponential

-- according to equality of a -> b there should be (total possible valus of type b) ^ (total possible values of type a ) implementation a function a -> b


-- binary tree

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' newData Leaf = Node Leaf newData Leaf
insert' newData (Node left a right)
  | newData == a = Node Leaf newData Leaf
  | newData < a  = Node (insert' newData left) a right
  | newData > a  = Node left a (insert' newData right)  


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = 
  Node (mapTree f left) (f a) (mapTree f right)


testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)


mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"
