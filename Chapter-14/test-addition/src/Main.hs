module Main where
import Addition
import Test.Hspec


-- :set -package hspec
{-
main :: IO ()
main = do
  num1 <- getLine
  num2 <- getLine 
  
  putStrLn (show $ add num1 num2)
-}

main :: IO ()
main = hspec $ do
  describe "Testing my addition function" $ do

    it "addition commutative property for integer" $ do
      add "2" "3" == add "3" "2" `shouldBe` True

    it "addition commutative property for doubles" $ do      
      add "2.5" "3" == add "3" "2.5" `shouldBe` True

    it "notifying message for both input string has alphabets" $ do      
      show (add "any" "any") == "Please enter only digits!" `shouldBe` True
    
    it "notifying message for one of the input string has alphabets" $ do      
      show (add "any" "12") == "Please enter only digits!" `shouldBe` True

    it "notifying message for one of the input string has alphabets" $ do      
      show (add "12" "any") == "Please enter only digits!" `shouldBe` True