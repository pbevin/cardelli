module AnalyzeSpec where

import Test.Hspec

import Data.Maybe
import Control.Monad.State
import qualified Data.Map as Map
import Analyze
import Parse
import Env
import Type
import Simplify

a = TypeVariable "a"
b = TypeVariable "b"

u = TypeVariable "u"
v = TypeVariable "v"
pair = funType u (funType v $ pairType u v)
w = TypeVariable "w"
idfunc = funType w w

fromLeft :: Either a b -> a
fromLeft (Left l) = l

fromRight :: Either String b -> b
fromRight (Right r) = r
fromRight (Left err) = error err

findType :: String -> TypeVars -> Type
findType str tv = fst $ fromRight $ runTypeInferencer tv str

mkTypeVars :: [(VarName, Type)] -> TypeVars
mkTypeVars varNames = foldl put initialVars varNames
  where put tv (a, t) = putEnv a t $ addNonGeneric t tv

typeOf :: String -> Type
typeOf str = simplify $ findType str initialVars

debug :: String -> IO ()
debug prog = putStrLn $ either ("Error: " ++) show $ runTypeInferencer initialVars prog

runTypeInferencer :: TypeVars -> String -> (Either String (Type, Env))
runTypeInferencer initialVars =
  runAnalyzer . analyzeExpr initialVars . parseFun

spec :: Spec
spec = do
  it "retrieves a variable's type from the env" $ do
    let vars = mkTypeVars [("n", TypeVariable "a")]
    findType "n" vars `shouldBe` TypeVariable "a"

  it "assigns int to a number" $ do
    typeOf "1" `shouldBe` int

  it "unifies types for a cond" $ do
    let vars = mkTypeVars [ ("x", TypeVariable "a"),
                            ("y", TypeVariable "b") ]
    findType "if x then 1 else y" vars `shouldBe` int
    findType "if x then y else 1" vars `shouldBe` int
    findType "if x then x else x" vars `shouldBe` bool

  it "analyzes a lambda" $ do
    typeOf "fun (x) x" `shouldBe` funType a a
    typeOf "fun (x) 1" `shouldBe` funType a int

  it "analyzes an int function" $ do
    typeOf "fun(x) x + 1" `shouldBe` funType int int

  it "analyzes fun(x) id(x)" $ do
    typeOf "fun(x) id(x)" `shouldBe` funType a a

  it "analyzes a heterogeneous function application" $ do
    typeOf "pair(id(3), id(true))" `shouldBe` pairType int bool
    typeOf "fun(x) pair(id(3), id(x))" `shouldBe` funType a (pairType int a)

  it "analyzes a function application" $ do
    typeOf "(fun (x) 1)(4)" `shouldBe` int
    typeOf "let y = 3 in (fun(x) x)(y)" `shouldBe` int

  it "analyzes a block" $ do
    typeOf "let y = 3 in y" `shouldBe` int
    typeOf "let y = 3 in let y = true in y" `shouldBe` bool

  it "analyzes a function application in a let block" $ do
    typeOf "let f = (fun(x) x) in f(3)" `shouldBe` int
    typeOf "let f = (fun(x) x) in f(true)" `shouldBe` bool

  it "analyzes a heterogeneous function application in a let block" $ do
    typeOf "let f = (fun(x) x) in pair(f(3), f(true))" `shouldBe` pairType int bool

  it "analyzes a simple let rec block" $ do
    typeOf "let rec f = fun (n) 3 in f" `shouldBe` funType a int

  it "analyzes a recursive let rec block" $ do
    typeOf "let rec f = fun(n) if zero(n) then 0 else f(n+1) in f" `shouldBe` funType int int

  it "analyzes a let..then block" $ do
    typeOf "let a=3 then b=true in pair(a,b)" `shouldBe` pairType int bool

  it "analyzes a recursive let..then" $ do
    typeOf "let rec (f = fun(n) 3 then g = fun(n) 4) in pair(f,g)"
      `shouldBe` pairType (funType a int) (funType b int)

  it "analyzes factorial" $ do
    let t = typeOf $ unlines
              [ "let rec factorial =",
                "  fun(n)",
                "    if n == 0",
                "    then 1",
                "    else n * factorial(n-1)",
                "in factorial" ]
    t `shouldBe` funType int int
