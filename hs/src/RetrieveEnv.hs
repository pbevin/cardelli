{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RetrieveEnv where

import Control.Monad.State
import qualified Data.Map as Map
import Type
import Env
import CopyType

retrieveEnv :: VarName -> TypeVars -> State Env (Maybe Type)
retrieveEnv a tv = do
  case Map.lookup a (vars tv) of
    Nothing -> return Nothing
    Just t' -> do
      env <- get
      let (copied, newEnv) = runCopier (copyType t' (ngvars tv)) env
      put newEnv
      return $ Just copied
