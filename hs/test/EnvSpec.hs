module EnvSpec where

import Control.Monad.State
import Test.Hspec
import VarName
import Type
import Env
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Env" $ do
    it "can get and put types" $ do
      let tv = putEnv (VarName "a") int $ putEnv (VarName "b") bool $ emptyVars
      getEnv (VarName "a") tv `shouldBe` Just int
      getEnv (VarName "b") tv `shouldBe` Just bool
      getEnv (VarName "c") tv `shouldBe` Nothing

    it "can generate a new variable" $ do
      runState newVar emptyEnv `shouldBe`
        (TypeVariable "a", Env 1 Map.empty)

    describe "varName" $ do
      it "runs through the alphabet" $ do
        map varName [0..5] `shouldBe` ["a", "b", "c", "d", "e", "f"]
