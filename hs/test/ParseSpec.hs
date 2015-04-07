module ParseSpec where

import Test.Hspec
import Test.QuickCheck

import Parse
import ShowExpr
import ASTGen
import AST

import Debug.Trace

spec :: Spec
spec = do
  describe "parseFun" $ do
    it "parses an identifier" $ do
      parseFun "x" `shouldBe` Var "x"

    it "parses a number" $ do
      parseFun "0" `shouldBe` Num 0

    it "parses a cond" $ do
      parseFun "if happy then 42 else 666" `shouldBe`
        Cond (Var "happy") (Num 42) (Num 666)

    it "parses a lambda" $ do
      parseFun "fun(x) x" `shouldBe`
        Lambda "x" (Var "x")

    it "parses a function application" $ do
      parseFun "f(1)" `shouldBe` FunCall (Var "f") (Num 1)

    it "parses a double function application" $ do
      parseFun "f(1)(2)" `shouldBe`
        FunCall (FunCall (Var "f") (Num 1)) (Num 2)

    it "parses a comma-ized function application" $ do
      parseFun "f(1, 2)" `shouldBe` parseFun "f(1)(2)"

    it "parses a let..in" $ do
      parseFun "let a = 2 in succ(a)" `shouldBe`
        Block (Assign "a" (Num 2)) (FunCall (Var "succ") (Var "a"))

    it "parses a let rec..in" $ do
      parseFun "let rec f = f(0) in f" `shouldBe`
        Block (Rec (Assign "f" (FunCall (Var "f") (Num 0)))) (Var "f")

    it "parses a seq" $ do
      parseFun "let a=3 then b=4 in a" `shouldBe`
        Block (Seq (Assign "a" (Num 3)) (Assign "b" (Num 4))) (Var "a")

    it "parses a parenthesized decl" $ do
      parseFun "let (a=3 then b=4) in a" `shouldBe`
        parseFun "let a=3 then b=4 in a"

    it "parses bracketed expressions" $ do
      parseFun "(if sad then f else g)(0)" `shouldBe`
        FunCall (Cond (Var "sad") (Var "f") (Var "g")) (Num 0)

    it "parses arithmetic and boolean operations" $ do
      parseFun "a+1" `shouldBe`
        FunCall (FunCall (Var "plus") (Var "a")) (Num 1)
      parseFun "a-1" `shouldBe`
        FunCall (FunCall (Var "minus") (Var "a")) (Num 1)
      parseFun "a*2" `shouldBe`
        FunCall (FunCall (Var "times") (Var "a")) (Num 2)
      parseFun "a/2" `shouldBe`
        FunCall (FunCall (Var "div") (Var "a")) (Num 2)
      parseFun "-a" `shouldBe` FunCall (Var "negate") (Var "a")
      parseFun "!b" `shouldBe` FunCall (Var "not") (Var "b")
      parseFun "happy & youknowit" `shouldBe`
        FunCall (FunCall (Var "and") (Var "happy")) (Var "youknowit")
      parseFun "b | !b" `shouldBe`
        FunCall (FunCall (Var "or") (Var "b")) (FunCall (Var "not") (Var "b"))

      parseFun "a+b*c" `shouldBe`
        FunCall (FunCall (Var "plus") (Var "a"))
                (FunCall (FunCall (Var "times") (Var "b")) (Var "c"))

      let ab = (FunCall (FunCall (Var "times") (Var "a")) (Var "b"))
      parseFun "a*b+c" `shouldBe`
        FunCall (FunCall (Var "plus") ab) (Var "c")

    it "is the opposite of showExpr" $ property $
      -- \expr -> length (showExpr expr) < 100
      \expr -> parseFun (showExpr expr) == expr
