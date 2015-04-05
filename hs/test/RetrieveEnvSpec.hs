module RetrieveEnvSpec where

import Control.Monad.State
import Test.Hspec
import Type
import Env
import RetrieveEnv


spec :: Spec
spec = do
  describe "RetrieveEnv" $ do
    context "for a generic type" $ do
      it "returns Nothing for a non-existent variable" $ do
        runState (retrieveEnv "a" emptyVars) emptyEnv `shouldBe`
          (Nothing, emptyEnv)

      it "returns a copy of a variable when it exists" $ do
        let test = do a <- newVar
                      retrieveEnv "x" $ putEnv "x" a emptyVars

        let (result, env) = runState test emptyEnv in do
          result `shouldBe` Just (TypeVariable "b")

      it "returns the original of a basic type" $ do
        let test = retrieveEnv "x" $ putEnv "x" int emptyVars

        let (result, env) = runState test emptyEnv in do
          result `shouldBe` Just int

      it "returns a copy of a type operator" $ do
        let test = do a <- newVar
                      b <- newVar
                      retrieveEnv "x" $ putEnv "x" (funType a b) emptyVars

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
                      retrieveEnv "map" $ putEnv "map" mapType emptyVars

        let (Just mapType, env) = runState test emptyEnv in do
          let c = TypeVariable "c"
          let d = TypeVariable "d"
          mapType `shouldBe` funType (funType c d) (funType (listType c) (listType d))

    context "For a non-generic type" $ do
      let vars = addNonGeneric (TypeVariable "a") emptyVars

      it "returns the original of a variable" $ do
        let test = do a <- newVar
                      let tv = putEnv "x" a vars
                      retrieveEnv "x" tv
        let (result, env) = runState test emptyEnv in do
          result `shouldBe` Just (TypeVariable "a")

      it "returns the original of a type operator" $ do
        let test = do a <- newVar
                      let tv = putEnv "f" (funType a a) vars
                      f <- retrieveEnv "f" tv
                      return (f, a)

        let ((f, a), env) = runState test emptyEnv in do
          f `shouldBe` Just (funType a a)