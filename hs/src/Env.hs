module Env where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Type

type VarName = String
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
addNonGeneric (TypeVariable a) tv = tv { ngvars = a:ngvars tv }
addNonGeneric _ tv = tv

getEnv :: VarName -> TypeVars -> Maybe Type
getEnv a tv = Map.lookup a $ vars tv

retrieveEnv :: VarName -> TypeVars -> State Env (Maybe Type)
retrieveEnv a tv = do
  case Map.lookup a (vars tv) of
    Nothing -> return Nothing
    Just t' -> do
      copied <- copyType t' (ngvars tv)
      return $ Just copied

copyType :: Type -> NonGenericVars -> State Env Type
copyType t ngvars = do
  (t, _) <- copyType' t ngvars Map.empty
  return t

type CopyMap = Map TypeName Type

copyType' :: Type -> NonGenericVars -> CopyMap -> State Env (Type, CopyMap)
copyType' t@(TypeVariable id) ngvars copyMap =
  if id `elem` ngvars
  then return (t, copyMap)
  else do
    case Map.lookup id copyMap of
      Nothing -> do
        t' <- newVar
        return (t', Map.insert id t' copyMap)
      Just t' -> return (t', copyMap)
copyType' t@(BasicType _) _ copyMap = return (t, copyMap)
copyType' t@(TypeOperator op args) ngvars copyMap = do
  (args', copyMap') <- copyArgs args ngvars copyMap
  return (TypeOperator op args', copyMap')

copyArgs :: [Type] -> NonGenericVars -> CopyMap -> State Env ([Type], CopyMap)
copyArgs [] ngvars copyMap = return ([], copyMap)
copyArgs (t:ts) ngvars copyMap = do
  (t', cm') <- copyType' t ngvars copyMap
  (ts', cm'') <- copyArgs ts ngvars cm'
  return (t':ts', cm'')
