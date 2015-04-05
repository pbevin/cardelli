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

    it "can get and put types" $ do
      let tv = putEnv "a" int $ putEnv "b" bool $ emptyVars
      getEnv "a" tv `shouldBe` Just int
      getEnv "b" tv `shouldBe` Just bool
      getEnv "c" tv `shouldBe` Nothing

    it "can generate a new variable" $ do
      runState newVar emptyEnv `shouldBe`
        (TypeVariable "a", Env 1 Map.empty)

    describe "varName" $ do
      it "runs through the alphabet" $ do
        map varName [0..5] `shouldBe` ["a", "b", "c", "d", "e", "f"]
