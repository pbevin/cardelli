module Unify where

import qualified Data.Map as Map
import Control.Monad
import Type
import Env

unify :: Type -> Type -> InstanceMap -> Maybe InstanceMap
unify t1 t2 m = unify' (prune m t1) (prune m t2) m

unify' :: Type -> Type -> InstanceMap -> Maybe InstanceMap
unify' t1 t2 m = case (t1, t2) of
  (TypeVariable id, _)
    | t1 /= t2 && prune m t1 `occursIn` prune m t2 -> Nothing
    | t1 == t2  -> Just m
    | otherwise -> Just $ Map.insert id t2 m

  (_, TypeVariable _) -> unify' t2 t1 m

  (BasicType _, BasicType _)
    | t1 == t2 -> Just m
    | otherwise -> Nothing

  (BasicType _, TypeOperator _ _) -> Nothing

  (TypeOperator _ _, BasicType _) -> Nothing

  (TypeOperator op1 args1, TypeOperator op2 args2)
    | op1 /= op2 -> Nothing
    | otherwise  -> unifyArgs args1 args2 m

unifyArgs :: [Type] -> [Type] -> InstanceMap -> Maybe InstanceMap
unifyArgs args1 args2 m
  | length args1 /= length args2 = Nothing
  | otherwise = foldM unifyPair m $ zip args1 args2

unifyPair :: InstanceMap -> (Type, Type) -> Maybe InstanceMap
unifyPair m (a1, a2) = liftM (Map.union m) $ unify a1 a2 m
