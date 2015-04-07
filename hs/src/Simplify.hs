module Simplify (simplify) where

import Type
import Control.Monad.State
import Data.Map
import qualified Data.Map as Map

type SimplifyEnv = (Int, Map TypeName TypeName)

simplify :: Type -> Type
simplify t = fst $ runState (simplify' t) (0, Map.empty)

simplify' :: Type -> State SimplifyEnv Type
simplify' t@(TypeVariable a) = do
  (_, m) <- get
  case Map.lookup (TypeName a) m of
    Nothing -> newType (TypeName a)
    Just a' -> return $ TypeVariable (fromTypeName a')

simplify' t@(BasicType a) = return t
simplify' t@(TypeOperator op args) = do
  (n, m) <- get
  args' <- mapM simplify' args
  return $ TypeOperator op args'


newType :: TypeName -> State SimplifyEnv Type
newType oldName = do
  (n, m) <- get
  let newName = TypeName (varName n)
  put $ (n+1, Map.insert oldName newName m)
  return $ TypeVariable (fromTypeName newName)
