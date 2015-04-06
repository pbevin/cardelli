{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FreshType (freshType, getFreshType) where

import Control.Monad.State
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map

import Type
import Env

freshType :: Type -> NonGenericVars -> State Env Type
freshType t ngvars =
  liftM fst $ runStateT (runCM copier) Map.empty
    where copier = copyType t ngvars

getFreshType :: Type -> Type
getFreshType t = fst $ runState (freshType t []) emptyEnv


type CopyMap = Map TypeName Type

newtype Copier a = Copier {
  runCM :: StateT CopyMap (State Env) a
} deriving (Monad, MonadState CopyMap, Functor, Applicative)

liftCT :: State Env a -> Copier a
liftCT m = Copier (lift m)

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
  case Map.lookup id copyMap of
    Just t -> return t
    Nothing -> do
      t <- liftCT newVar
      put $ Map.insert id t copyMap
      return t

copyArgs :: [Type] -> NonGenericVars -> Copier [Type]
copyArgs ts ngvars = mapM ct ts
  where ct t = copyType t ngvars
