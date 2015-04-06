{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RetrieveEnv where

import Control.Monad.State
import Control.Applicative
import Type
import Data.Map (Map)
import qualified Data.Map as Map
import Env

retrieveEnv :: VarName -> TypeVars -> State Env (Maybe Type)
retrieveEnv a tv = do
  case Map.lookup a (vars tv) of
    Nothing -> return Nothing
    Just t' -> do
      env <- get
      let (copied, newEnv) = runCopier (copyType t' (ngvars tv)) env
      put newEnv
      return $ Just copied

type CopyMap = Map TypeName Type

newtype Copier a = Copier {
  runCM :: StateT CopyMap (State Env) a
} deriving (Monad, MonadState CopyMap, Functor, Applicative)

runCopier :: Copier a -> Env -> (a, Env)
runCopier a env = (t, e')
  where ((t, _), e') = runState (runStateT (runCM a) Map.empty) env

createType :: Copier Type
createType = Copier (lift newVar)

copyType :: Type -> NonGenericVars -> Copier Type
copyType t ngvars = case t of
  TypeVariable id
    | id `elem` ngvars -> return t
    | otherwise        -> copyTypeVariable id
  BasicType _ -> return t
  TypeOperator op args -> copyArgs args ngvars >>= return . TypeOperator op

copyTypeVariable :: TypeName -> Copier Type
copyTypeVariable id = do
  copyMap <- get
  maybe (createIn copyMap) return $ Map.lookup id copyMap
    where createIn copyMap = do
            t <- createType
            put $ Map.insert id t copyMap
            return t

copyArgs :: [Type] -> NonGenericVars -> Copier [Type]
copyArgs ts ngvars = mapM ct ts
  where ct t = copyType t ngvars
