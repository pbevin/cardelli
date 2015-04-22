module EvalSpec where

import Test.Hspec
import AST
import Eval
import Parse

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates a number" $ do
      eval (Num 5) `shouldBe` VNum 5
      eval (Num 0) `shouldBe` VNum 0

    it "evaluates arithmetic" $ do
      eval (parseFun "3+4") `shouldBe` VNum 7
      eval (parseFun "3+4*5") `shouldBe` VNum 23
      eval (parseFun "3*4+5") `shouldBe` VNum 17
      eval (parseFun "10/3") `shouldBe` VNum 3

    it "evaluates booleans" $ do
      eval (parseFun "3 == 3") `shouldBe` VBool True
      eval (parseFun "3 == 4") `shouldBe` VBool False

    it "evaluates user-defined functions" $ do
      eval (parseFun "let f = fun(x) x+1 in f(3)") `shouldBe` VNum 4

    it "evaluates factorial" $ do
      let expr = unlines
                [ "let rec factorial =",
                  "  fun(n)",
                  "    if n == 0",
                  "    then 1",
                  "    else n * factorial(n-1)",
                  "in factorial(6)" ]
      eval (parseFun expr) `shouldBe` VNum 720
