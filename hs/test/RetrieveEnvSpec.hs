module RetrieveEnvSpec where

import Control.Monad.State
import Test.Hspec
import VarName
import Type
import Env
import RetrieveEnv
import ParseType

spec :: Spec
spec = do
  describe "RetrieveEnv" $ do
    context "for a generic type" $ do
      it "returns Nothing for a non-existent variable" $ do
        runState (retrieveEnv (VarName "a") emptyVars) emptyEnv `shouldBe`
          (Nothing, emptyEnv)

      it "returns a copy of a variable when it exists" $ do
        let test = do a <- newVar
                      retrieveEnv (VarName "x") $ putEnv (VarName "x") a emptyVars

        let (result, env) = runState test emptyEnv in do
          result `shouldBe` Just (TypeVariable "b")

      it "returns the original of a basic type" $ do
        let test = retrieveEnv (VarName "x") $ putEnv (VarName "x") int emptyVars

        let (result, env) = runState test emptyEnv in do
          result `shouldBe` Just int

      it "returns a copy of a type operator" $ do
        let test = do a <- newVar
                      b <- newVar
                      retrieveEnv (VarName "x") $ putEnv (VarName "x") (parseType "a -> b") emptyVars

        let (Just x, env) = runState test emptyEnv in do
          x `shouldBe` parseType "c -> d"

      it "retains the identity of repeated variables" $ do
        -- map :: (a->b) -> [a] -> [b] should be copied as
        -- map :: (c->d) -> [c] -> [d]
        let test = do a <- newVar
                      b <- newVar
                      let mapType = parseType "(a -> b) -> [a] -> [b]"
                      retrieveEnv (VarName "map") $ putEnv (VarName "map") mapType emptyVars

        let (Just mapType, env) = runState test emptyEnv in do
          mapType `shouldBe` parseType "(c -> d) -> [c] -> [d]"

    context "For a non-generic type" $ do
      let vars = addNonGeneric (TypeVariable "a") emptyVars

      it "returns the original of a variable" $ do
        let test = do a <- newVar
                      let tv = putEnv (VarName "x") a vars
                      retrieveEnv (VarName "x") tv
        let (result, env) = runState test emptyEnv in do
          result `shouldBe` Just (TypeVariable "a")

      it "returns the original of a type operator" $ do
        let test = do a <- newVar
                      let tv = putEnv (VarName "f") (parseType "a -> a") vars
                      f <- retrieveEnv (VarName "f") tv
                      return (f, a)

        let ((f, a), env) = runState test emptyEnv in do
          f `shouldBe` Just (parseType "a -> a")
