module UnifySpec where

import Test.Hspec
import qualified Data.Map as Map
import Type
import ParseType
import Unify

success vs = Just $ Map.fromList $ map tt vs
  where tt (name, typeStr) = (name, parseType typeStr)


spec :: Spec
spec = do
  describe "unify" $ do
    let u t1 t2 = u' t1 t2 []
        u' t1 t2 ngvars = unify (parseType t1) (parseType t2) $ Map.fromList ngvars

    it "unifies a with int" $ do
      u "a" "Int" `shouldBe` success [("a", "Int")]

    it "unifies a with b" $ do
      u "a" "b" `shouldBe` success [("a", "b")]

    it "unifies b with a" $ do
      u "b" "a" `shouldBe` success [("b", "a")]

    it "unifies a with a" $ do
      u "a" "a" `shouldBe` success []

    it "does not unify a variable with a list" $ do
      u "a" "[a]" `shouldBe` Nothing

    it "unifies a->b with int->bool" $ do
      u "a -> b" "Int -> Bool" `shouldBe` success [("a", "Int"), ("b", "Bool")]

    it "does not unify a->a with int->bool" $ do
      u "a -> a" "Int -> Bool" `shouldBe` Nothing

    it "does not generalize a->a across two sides of an outer ->" $ do
      u "(a->a)->(a->a)" "(Int->Int)->(Bool->Bool)" `shouldBe` Nothing

    it "unifies variables on both sides at once" $ do
      u "a -> Bool" "Int -> b" `shouldBe` success [("a", "Int"), ("b", "Bool")]

    it "does not unify types with different structures" $ do
      u "int -> int" "int -> int -> int" `shouldBe` Nothing

    it "unifies a variable with another variable" $ do
      u "Int -> a" "Int -> b" `shouldBe` success [("a", "b")]

    it "does not unify a with int when a already has instance bool" $ do
      u' "a" "Int" [("a", bool)] `shouldBe` Nothing
