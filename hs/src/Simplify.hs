module Simplify (simplify) where

import Type
import Control.Monad.State
import Data.Map
import qualified Data.Map as Map

type SimplifyEnv = (Int, Map String String)

simplify :: Type -> Type
simplify t = fst $ runState (simplify' t) (0, Map.empty)

simplify' :: Type -> State (Int, Map String String) Type
simplify' t@(TypeVariable a) = do
  (n, m) <- get
  case Map.lookup a m of
    Nothing -> newType a
    Just a' -> return $ TypeVariable a'

simplify' t@(BasicType a) = return t
simplify' t@(TypeOperator op args) = do
  (n, m) <- get
  args' <- mapM simplify' args
  return $ TypeOperator op args'


newType :: String -> State SimplifyEnv Type
newType oldName = do
  (n, m) <- get
  let newName = varName n
  put $ (n+1, Map.insert oldName newName m)
  return $ TypeVariable newName
