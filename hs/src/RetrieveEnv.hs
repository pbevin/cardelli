{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RetrieveEnv where

import Control.Monad.State
import qualified Data.Map as Map
import VarName
import Type
import TypeEnv
import FreshType

retrieveEnv :: VarName -> TypeVars -> State TypeEnv (Maybe Type)
retrieveEnv a tv = do
  case Map.lookup a (vars tv) of
    Nothing -> return Nothing
    Just t' -> liftM Just $ freshType t' (ngvars tv)
