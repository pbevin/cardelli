module RetrieveEnv where

import Control.Monad.State
import Type
import Data.Map (Map)
import qualified Data.Map as Map
import Env

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
