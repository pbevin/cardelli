module ParseTypeSpec where

import Test.Hspec
import Test.QuickCheck
import Type
import ParseType
import TypeGen

spec :: Spec
spec = do
  it "parses a basic type" $ do
    parseType "Int" `shouldBe` BasicType "Int"
    parseType "Bool" `shouldBe` BasicType "Bool"

  it "parses a generic type" $ do
    parseType "a" `shouldBe` TypeVariable "a"
    parseType "xyz" `shouldBe` TypeVariable "xyz"
    parseType "_xyz" `shouldBe` TypeVariable "_xyz"

  it "parses a list type" $ do
    parseType "[a]" `shouldBe` TypeOperator "[]" [TypeVariable "a"]
    parseType "[ a ]" `shouldBe` TypeOperator "[]" [TypeVariable "a"]
    parseType "[[a]]" `shouldBe` TypeOperator "[]" [TypeOperator "[]" [TypeVariable "a"]]

  it "parses a tuple type" $ do
    parseType "(Int,Bool)" `shouldBe` TypeOperator "," [BasicType "Int", BasicType "Bool"]

  it "only parses a list type if there are commas" $ do
    parseType "(a)" `shouldBe` TypeVariable "a"

  it "parses a function type" $ do
    parseType "Int -> Int" `shouldBe` TypeOperator "->" [BasicType "Int", BasicType "Int"]
    parseType "a -> b -> c" `shouldBe` TypeOperator "->" [TypeVariable "a", TypeOperator "->" [TypeVariable "b", TypeVariable "c"]]
    parseType "(a->b)->c" `shouldBe` TypeOperator "->" [TypeOperator "->" [TypeVariable "a", TypeVariable "b"], TypeVariable "c"]

  it "parses combinations of operators" $ do
    parseType "[a->a]" `shouldBe` TypeOperator "[]" [TypeOperator "->" [TypeVariable "a", TypeVariable "a"]]
    parseType "a -> [a]" `shouldBe` TypeOperator "->" [TypeVariable "a", TypeOperator "[]" [TypeVariable "a"]]

  it "is inverse to show" $ property $
    \x -> (parseType . show) x == x
