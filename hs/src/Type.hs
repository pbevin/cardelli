module Type where

import Data.Char
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

newtype TypeName = TypeName { fromTypeName :: String }
  deriving (Eq, Ord)

data Type = TypeVariable String
          | BasicType String
          | TypeOperator String [Type]
   deriving Eq

type InstanceMap = Map TypeName Type

instance Show TypeName where
  show (TypeName a) = a

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

int = BasicType "Int"
bool = BasicType "Bool"
funType a b = TypeOperator "->" [a,b]
pairType a b = TypeOperator "," [a,b]
listType a = TypeOperator "[]" [a]

typeName :: Type -> TypeName
typeName (TypeVariable a) = TypeName a

typeNameMap :: [(String, b)] -> Map TypeName b
typeNameMap = Map.fromList . map tt
  where tt (a,b) = (TypeName a, b)

prune :: InstanceMap -> Type -> Type
prune m t@(TypeVariable name) =
  case (Map.lookup (TypeName name) m) of
    Nothing -> t
    Just t' -> prune m t'
prune m t@(TypeOperator op args) =
  TypeOperator op $ map (prune m) args
prune _ any = any

varName :: Int -> String
varName n = [chr $ n + ord 'a']

occursIn :: Type -> Type -> Bool
occursIn t1 (TypeOperator _ args) = t1 `occursInList` args
  where occursInList t ts = any occurrence ts
          where occurrence t2 = t `occursIn` t2
occursIn t1 t2 = t1 == t2
