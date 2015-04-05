module Type where

import Data.Char
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

type TypeName = String

data Type = TypeVariable TypeName
          | BasicType String
          | TypeOperator String [Type]
   deriving Eq

type InstanceMap = Map TypeName Type

instance Show Type where
  show (TypeVariable a) = a
  show (BasicType a) = a
  show (TypeOperator op args)
    | op == "[]" = "[" ++ (intercalate "," $ map show args) ++ "]"
    | op == "->" = showFuncType args
    | otherwise  = "(" ++ (intercalate op $ map show args) ++ ")"

showFuncType :: [Type] -> String
showFuncType args = intercalate "->" $ map sh (init args) ++ [show (last args)]
  where sh t@(TypeOperator op _)
          | op == "->" = "(" ++ (show t) ++ ")"
          | otherwise  = show t
        sh t = show t

int = BasicType "int"
bool = BasicType "bool"
funType a b = TypeOperator "->" [a,b]
pairType a b = TypeOperator "," [a,b]
listType a = TypeOperator "[]" [a]

typeName :: Type -> TypeName
typeName (TypeVariable a) = a

prune :: Type -> InstanceMap -> Type
prune t@(TypeVariable name) m =
  case (Map.lookup name m) of
    Nothing -> t
    Just t' -> prune t' m
prune t@(TypeOperator op args) m =
  TypeOperator op $ map (\a -> prune a m) args
prune any _ = any

varName :: Int -> String
varName n = [chr $ n + ord 'a']

occursIn :: Type -> Type -> Bool
occursIn t1 (TypeOperator _ args) = t1 `occursInList` args
  where occursInList t ts = any occurrence ts
          where occurrence t2 = t `occursIn` t2
occursIn t1 t2 = t1 == t2
