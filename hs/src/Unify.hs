module Unify where

import qualified Data.Map as Map
import Control.Monad
import Type
import Env

unify :: Type -> Type -> InstanceMap -> Maybe InstanceMap
unify t1 t2 m = unify' (prune m t1) (prune m t2) m

unify' :: Type -> Type -> InstanceMap -> Maybe InstanceMap
unify' t1@(TypeVariable id) t2 m
  | t1 /= t2 && prune m t1 `occursIn` prune m t2 = Nothing
  | t1 == t2  = Just m
  | otherwise = Just $ Map.insert id t2 m
unify' t1 t2@(TypeVariable _) m = unify' t2 t1 m
unify' t1@(BasicType b1) t2@(BasicType b2) m
  | b1 == b2  = Just m
  | otherwise = Nothing
unify' t1@(BasicType _) t2@(TypeOperator _ _) _ = Nothing
unify' t1@(TypeOperator _ _) t2@(BasicType _) _ = Nothing
unify' t1@(TypeOperator op1 args1) t2@(TypeOperator op2 args2) m
  | op1 /= op2 = Nothing
  | otherwise  = unifyArgs args1 args2 m



unifyArgs :: [Type] -> [Type] -> InstanceMap -> Maybe InstanceMap
unifyArgs args1 args2 m =
  if length args1 /= length args2
  then Nothing
  else liftM fst $ foldM unifyPair (Map.empty, m) $ zip args1 args2

type UnifyContext = (InstanceMap, InstanceMap)
unifyPair :: UnifyContext -> (Type, Type) -> Maybe UnifyContext
unifyPair (xs, m) (a1, a2) = liftM updateContext $ unify a1 a2 m
  where updateContext :: InstanceMap -> UnifyContext
        updateContext ys = (Map.union xs ys, Map.union m ys)
