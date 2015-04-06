module FreshTypeSpec where

import Control.Monad.State
import Test.Hspec
import Test.QuickCheck
import Type
import Unify
import FreshType
import Simplify
import TypeGen

spec :: Spec
spec = do
  it "is unifiable with its original" $ property $
    \t -> getFreshType t `unifiable` t

  it "simplifies to the same as its original" $ property $
    \t -> simplify (getFreshType t) == simplify t
