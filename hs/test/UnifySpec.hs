module UnifySpec where

import Test.Hspec
import qualified Data.Map as Map
import Type
import Unify

spec :: Spec
spec = do
  let a = TypeVariable "a"
  let b = TypeVariable "b"
  let int = BasicType "int"
  let bool = BasicType "bool"
  let aList = listType a

  describe "unify" $ do
    it "unifies a with int" $ do
      unify a int Map.empty `shouldBe` Just [("a", int)]

    it "unifies a with b" $ do
      unify a b Map.empty `shouldBe` Just [("a", b)]

    it "unifies b with a" $ do
      unify b a Map.empty `shouldBe` Just [("b", a)]

    it "unifies a with a" $ do
      unify a a Map.empty `shouldBe` Just []

    it "does not unify a with [a]" $ do
      unify a aList Map.empty `shouldBe` Nothing

    it "unifies a->b with int->bool" $ do
      let f = funType a b
      let g = funType int bool
      unify f g Map.empty `shouldBe` Just [("a", int), ("b", bool)]

    it "does not unify a->a with int->bool" $ do
      let f = funType a a
      let g = funType int bool
      unify f g Map.empty `shouldBe` Nothing

    it "does not unify (a->a)->(a->a) with (int->int)->(bool->bool)" $ do
      let lhs = funType (funType a a) (funType a a)
      let rhs = funType (funType int int) (funType bool bool)
      unify lhs rhs Map.empty `shouldBe` Nothing

    it "unifies a->bool with int->b" $ do
      let f = funType a bool
      let g = funType int b
      unify f g Map.empty `shouldBe` Just [("a", int), ("b", bool)]

    it "does not unify int->int with int->int->int" $ do
      let plus = funType int (funType int int)
      let succ = funType int int
      unify plus succ Map.empty `shouldBe` Nothing

    it "unifies int->a with int->b" $ do
      let f = funType int a
      let g = funType int b
      unify f g Map.empty `shouldBe` Just [("a", b)]

    it "does not unify a with int when a already has instance bool" $ do
      unify a int (Map.fromList [("a", bool)]) `shouldBe` Nothing
