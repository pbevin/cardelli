module SimplifySpec where

import Test.Hspec

import ParseType
import Simplify

spec :: Spec
spec = do
  describe "simplify" $ do
    it "renames type variables" $ do
      simplify (parseType "a") `shouldBe` (parseType "a")
      simplify (parseType "b") `shouldBe` (parseType "a")
      simplify (parseType "Int") `shouldBe` (parseType "Int")
      simplify (parseType "k -> m") `shouldBe` (parseType "a -> b")
      simplify (parseType "k -> k") `shouldBe` (parseType "a -> a")
