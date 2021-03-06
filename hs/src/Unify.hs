module Unify (unify, unifiable) where

import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
import Type
import TypeEnv

unify :: Type -> Type -> InstanceMap -> Maybe InstanceMap
unify t1 t2 m = unify' (prune m t1) (prune m t2) m

unifiable :: Type -> Type -> Bool
unifiable t1 t2 = isJust (unify t1 t2 Map.empty)

unify' :: Type -> Type -> InstanceMap -> Maybe InstanceMap
unify' t1 t2 m = case (t1, t2) of
  (TypeVariable id, _)
    | t1 /= t2 && prune m t1 `occursIn` prune m t2 -> Nothing
    | t1 == t2  -> Just m
    | otherwise -> Just $ Map.insert (TypeName id) t2 m

  (BasicType _, BasicType _)
    | t1 == t2 -> Just m
    | otherwise -> Nothing

  (TypeOperator op1 args1, TypeOperator op2 args2)
    | op1 /= op2 -> Nothing
    | otherwise  -> unifyArgs args1 args2 m

  (_, TypeVariable _) -> unify' t2 t1 m
  ( _, _) -> Nothing

unifyArgs :: [Type] -> [Type] -> InstanceMap -> Maybe InstanceMap
unifyArgs [] [] m = Just m
unifyArgs (x:xs) (y:ys) m = unify x y m >>= unifyArgs xs ys
unifyArgs _ _ _ = Nothing
