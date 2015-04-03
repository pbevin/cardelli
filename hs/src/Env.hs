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
  vars :: VarMap,
  ngvars :: NonGenericVars,
  instances :: InstanceMap
} deriving (Eq, Show)

emptyEnv :: Env
emptyEnv = Env 0 Map.empty [] Map.empty

newVar :: State Env Type
newVar = do
  e <- get
  let n = counter e
  put $ e { counter = (n + 1) }
  return $ TypeVariable (varName n)

putEnv :: VarName -> Type -> State Env ()
putEnv a t = do
  e <- get
  put $ e { vars = Map.insert a t $ vars e }
  return ()

getEnv :: VarName -> State Env (Maybe Type)
getEnv a = get >>= return . Map.lookup a . vars

retrieveEnv :: VarName -> State Env (Maybe Type)
retrieveEnv a = do
  e <- get
  case Map.lookup a (vars e) of
    Nothing -> return Nothing
    Just t' -> do
      copied <- copyType t' (ngvars e)
      return $ Just copied

addGeneric :: Type -> State Env ()
addGeneric (TypeVariable id) = do
  e <- get
  put $ e { ngvars = id:ngvars e }
  return ()
addGeneric t = return ()

addNonGenericVar :: VarName -> Type -> State Env ()
addNonGenericVar a t = do
  putEnv a t
  addGeneric t

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
