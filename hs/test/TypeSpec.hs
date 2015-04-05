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
      prune Map.empty a `shouldBe` a

    it "returns its instance when there is one" $ do
      prune (Map.fromList [("b", a)]) b `shouldBe` a

    it "returns the bottom level when more than 2" $ do
      prune (Map.fromList [("c", b), ("b", a)]) c `shouldBe` a

    it "returns a BasicType as-is" $ do
      prune Map.empty int `shouldBe` int
      prune (Map.fromList [("a", int)]) a `shouldBe` int

    it "returns a TypeOperator as-is" $ do
      prune Map.empty e `shouldBe` e

  describe "occursIn" $ do
    it "is true when types are the same" $ do
      int `occursIn` int `shouldBe` True
      a `occursIn` a `shouldBe` True

    it "is false for two different basic types" $ do
      int `occursIn` bool `shouldBe` False
      bool `occursIn` int `shouldBe` False
      a `occursIn` b `shouldBe` False

    it "is true when t1 is in t2's operands" $ do
      a `occursIn` e `shouldBe` True
      int `occursIn` e `shouldBe` True

    it "is false when t1 is not in t2's operands" $ do
      bool `occursIn` e `shouldBe` False

  describe "show" $ do
    it "shows operator types correctly" $ do
      show (funType a a) `shouldBe` "a->a"
      show (funType (funType a b) a) `shouldBe` "(a->b)->a"
      show (funType a (funType b a)) `shouldBe` "a->b->a"
      show (funType a (pairType b a)) `shouldBe` "a->(b,a)"
      show (funType (listType a) a) `shouldBe` "[a]->a"
