module TypeSpec where

import Test.Hspec

import qualified Data.Map as Map
import Control.Monad.State
import Type
import Env

spec :: Spec
spec = do
  let int  = BasicType "int"
      bool = BasicType "bool"
      a    = TypeVariable "a"
      b    = TypeVariable "b"
      c    = TypeVariable "c"
      d    = TypeVariable "d"
      e    = funType a int
      f    = TypeVariable "f"

  describe "prune" $ do
    it "returns a variable with no instance as-is" $ do
      prune a Map.empty `shouldBe` a

    it "returns its instance when there is one" $ do
      prune b (Map.fromList [("b", a)]) `shouldBe` a

    it "returns the bottom level when more than 2" $ do
      prune c (Map.fromList [("c", b), ("b", a)]) `shouldBe` a

    it "returns a BasicType as-is" $ do
      prune int Map.empty `shouldBe` int
      prune a (Map.fromList [("a", int)]) `shouldBe` int

    it "returns a TypeOperator as-is" $ do
      prune e Map.empty `shouldBe` e

  describe "occursIn" $ do
    it "is true when types are the same" $ do
      int `occursIn` (int, Map.empty) `shouldBe` True
      a `occursIn` (a, Map.empty) `shouldBe` True
      a `occursIn` (a, Map.fromList [("a", int)]) `shouldBe` True

    it "is false when the types are different" $ do
      int `occursIn` (bool, Map.empty) `shouldBe` False
      bool `occursIn` (int, Map.empty) `shouldBe` False
      a `occursIn` (b, Map.empty) `shouldBe` False

    it "is true when a type resolves to another type" $ do
      a `occursIn` (b, Map.fromList [("a", b)]) `shouldBe` True
      a `occursIn` (b, Map.fromList [("b", a)]) `shouldBe` True

    it "is true when t1 is in t2's operands" $ do
      a `occursIn` (e, Map.empty) `shouldBe` True
      b `occursIn` (e, Map.fromList [("b", a)]) `shouldBe` True
      int `occursIn` (e, Map.empty) `shouldBe` True

    it "is false when t1 is not in t2's operands" $ do
      bool `occursIn` (e, Map.empty) `shouldBe` False

  describe "show" $ do
    it "shows operator types correctly" $ do
      show (funType a a) `shouldBe` "a->a"
      show (funType (funType a b) a) `shouldBe` "(a->b)->a"
      show (funType a (funType b a)) `shouldBe` "a->b->a"
      show (funType a (pairType b a)) `shouldBe` "a->(b,a)"
      show (funType (listType a) a) `shouldBe` "[a]->a"
