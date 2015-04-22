module Eval where

import Text.Show.Functions
import Data.Map (Map)
import qualified Data.Map as Map
import VarName

import AST

instance Eq (a -> b) where
  f == g = False

data Val = VNum Integer | VBool Bool | VFun (ValueEnv -> Val -> Val) deriving (Eq, Show)

type Ident = String
type ValueEnv = Map Ident Val

eval :: Expr -> Val
eval = ev prelude

ev :: ValueEnv -> Expr -> Val
ev env e = case e of
  Num x -> VNum x

  Var x -> case Map.lookup x env of
    Just v -> v
    Nothing -> error $ "No such variable: " ++ x

  FunCall f x -> case (ev env f) of
    VFun f -> f env (ev env x)
    _ -> error "Not a function"

  Block decl e ->
    let env' = evalDecl env decl
    in ev env' e

  Lambda var e -> VFun f
    where
      f env x = ev (Map.insert var x env) e

  Cond test e1 e2 ->
    let p = ev env test
    in case (ev env test) of
         VBool True  -> ev env e1
         VBool False -> ev env e2
         _           -> error $ "Not a boolean: " ++ (show $ ev env test)


evalDecl :: ValueEnv -> Decl -> ValueEnv
evalDecl env d = case d of
  Assign var e ->
    let v = ev env e
    in Map.insert var v env

  Rec d -> evalDecl env d

prelude :: ValueEnv
prelude = Map.fromList [ ("plus", binary plus),
                         ("times", binary times),
                         ("minus", binary minus),
                         ("div", binary idiv),

                         ("eq", binary eq) ]

binary :: (Val -> Val -> Val) -> Val
binary f = VFun (\e a -> VFun $ \e b -> f a b)

plus  (VNum a) (VNum b) = VNum (a+b)
minus (VNum a) (VNum b) = VNum (a-b)
times (VNum a) (VNum b) = VNum (a*b)
idiv  (VNum a) (VNum b) = VNum (a `div` b)
eq    (VNum a) (VNum b) = VBool (a == b)
