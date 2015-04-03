module SimplifySpec where

import Test.Hspec

import Type
import Simplify

a = TypeVariable "a"
b = TypeVariable "b"
k = TypeVariable "k"
m = TypeVariable "m"

spec :: Spec
spec = do
  describe "simplify" $ do
    it "renames type variables" $ do
      simplify a `shouldBe` a
      simplify b `shouldBe` a
      simplify int `shouldBe` int
      simplify (funType k m) `shouldBe` (funType a b)
      simplify (funType k k) `shouldBe` (funType a a)
