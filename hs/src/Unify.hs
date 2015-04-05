module Unify where

import qualified Data.Map as Map
import Control.Monad
import Type
import Env

unify :: Type -> Type -> InstanceMap -> Maybe [(VarName, Type)]
unify t1 t2 m = unify' (prune t1 m) (prune t2 m) m

unify' :: Type -> Type -> InstanceMap -> Maybe [(VarName, Type)]
unify' t1@(TypeVariable id) t2 m
  | t1 /= t2 && prune t1 m `occursIn` prune t2 m = Nothing
  | t1 == t2  = Just []
  | otherwise = Just [(id, t2)]
unify' t1 t2@(TypeVariable _) m = unify' t2 t1 m
unify' t1@(BasicType b1) t2@(BasicType b2) _
  | b1 == b2  = Just []
  | otherwise = Nothing
unify' t1@(BasicType _) t2@(TypeOperator _ _) _ = Nothing
unify' t1@(TypeOperator _ _) t2@(BasicType _) _ = Nothing
unify' t1@(TypeOperator op1 args1) t2@(TypeOperator op2 args2) m
  | op1 /= op2 = Nothing
  | otherwise  = unifyArgs args1 args2 m


type UnifyContext = ([(VarName, Type)], InstanceMap)

unifyArgs :: [Type] -> [Type] -> InstanceMap -> Maybe [(VarName, Type)]
unifyArgs args1 args2 m =
  if length args1 /= length args2
  then Nothing
  else liftM fst $ foldM unifyPair ([], m) $ zip args1 args2

unifyPair :: UnifyContext -> (Type, Type) -> Maybe UnifyContext
unifyPair (xs, m) (a1, a2) = liftM updateContext $ unify a1 a2 m
  where updateContext :: [(VarName, Type)] -> UnifyContext
        updateContext ys = (xs ++ ys, Map.union m $ Map.fromList ys)
