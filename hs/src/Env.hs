module Env where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import VarName
import Type

type NonGenericVars = [TypeName]

type VarCounter  = Int
type VarMap      = Map VarName Type

data Env = Env {
  counter :: VarCounter,
  instances :: InstanceMap
} deriving (Eq, Show)

data TypeVars = TypeVars {
  vars :: VarMap,
  ngvars :: NonGenericVars
} deriving (Eq, Show)

emptyEnv :: Env
emptyEnv = Env 0 Map.empty

emptyVars :: TypeVars
emptyVars = TypeVars Map.empty []

newVar :: State Env Type
newVar = do
  e <- get
  let n = counter e
  put $ e { counter = (n + 1) }
  return $ TypeVariable (varName n)

putEnv :: VarName -> Type -> TypeVars -> TypeVars
putEnv a t tv = tv { vars = Map.insert a t (vars tv) }

addNonGeneric :: Type -> TypeVars -> TypeVars
addNonGeneric (TypeVariable a) tv = tv { ngvars = (TypeName a):ngvars tv }
addNonGeneric _ tv = tv

getEnv :: VarName -> TypeVars -> Maybe Type
getEnv a tv = Map.lookup a $ vars tv
