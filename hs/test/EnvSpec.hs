module EnvSpec where

import Control.Monad.State
import Test.Hspec
import Type
import Env
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Env" $ do
    let int = BasicType "int"
    let bool = BasicType "int"

    it "remembers a type definition" $ do
      runState (putEnv "n" int) emptyEnv `shouldBe`
        ((), Env 0 (Map.fromList [("n", int)]) [] Map.empty)

    it "can retrieve a type definition" $ do
      let env = Env 0 (Map.fromList [("n", int)]) [] Map.empty
      runState (getEnv "n") env `shouldBe` (Just int, env)

    it "can get and put a type in a monad" $ do
      let test = do putEnv "a" int
                    putEnv "b" bool
                    a <- getEnv "a"
                    b <- getEnv "b"
                    c <- getEnv "c"
                    return (a, b, c)

      let (result, env) = runState test emptyEnv in do
        result `shouldBe` (Just int, Just bool, Nothing)

    it "can generate a new variable" $ do
      runState newVar emptyEnv `shouldBe`
        (TypeVariable "a", Env 1 Map.empty [] Map.empty)

    describe "varName" $ do
      it "runs through the alphabet" $ do
        map varName [0..5] `shouldBe` ["a", "b", "c", "d", "e", "f"]

    describe "retrieveEnv" $ do
      context "for a generic type" $ do
        it "returns Nothing for a non-existent variable" $ do
          runState (retrieveEnv "a") emptyEnv `shouldBe`
            (Nothing, emptyEnv)

        it "returns a copy of a variable when it exists" $ do
          let test = do a <- newVar
                        putEnv "x" a
                        retrieveEnv "x"

          let (result, env) = runState test emptyEnv in do
            result `shouldBe` Just (TypeVariable "b")

        it "returns the original of a basic type" $ do
          let test = do putEnv "x" int
                        retrieveEnv "x"

          let (result, env) = runState test emptyEnv in do
            result `shouldBe` Just int

        it "returns a copy of a type operator" $ do
          let test = do a <- newVar
                        b <- newVar
                        putEnv "x" $ funType a b
                        retrieveEnv "x"

          let (Just x, env) = runState test emptyEnv in do
            let c = TypeVariable "c"
            let d = TypeVariable "d"
            x `shouldBe` funType c d

        it "retains the identity of repeated variables" $ do
          -- map :: (a->b) -> [a] -> [b] should be copied as
          -- map :: (c->d) -> [c] -> [d]
          let test = do a <- newVar
                        b <- newVar
                        let aTob    = funType a b
                        let listOfa = listType a
                        let listOfb = listType b
                        let mapType = funType aTob $ funType listOfa listOfb
                        putEnv "map" mapType
                        retrieveEnv "map"

          let (Just mapType, env) = runState test emptyEnv in do
            let c = TypeVariable "c"
            let d = TypeVariable "d"
            mapType `shouldBe` funType (funType c d) (funType (listType c) (listType d))

      context "For a non-generic type" $ do
        it "returns the original of a variable" $ do
          let test = do a <- newVar
                        addGeneric a
                        putEnv "x" a
                        retrieveEnv "x"
          let (result, env) = runState test emptyEnv in do
            result `shouldBe` Just (TypeVariable "a")

        it "returns the original of a type operator" $ do
          let test = do a <- newVar
                        addGeneric a
                        putEnv "f" $ funType a a
                        Just f <- retrieveEnv "f"
                        return (f, a)

          let ((f, a), env) = runState test emptyEnv in do
            f `shouldBe` funType a a
