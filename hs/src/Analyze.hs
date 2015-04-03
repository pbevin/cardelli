{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyze (runAnalyzer, analyzeExpr, addEnv, addEnvUnknown) where

import Control.Monad.State
import Control.Monad.Error
import Control.Applicative (Applicative)
import qualified Data.Map as Map
import Data.Maybe
import AST
import Parse
import Env
import Type
import Unify

newtype Analyzer a = Analyzer {
  runA :: ErrorT String (State Env) a
} deriving (Monad, MonadError String, Functor, Applicative)

basicEnv :: Env
basicEnv = emptyEnv { vars = basicVars }
basicVars = Map.fromList
  [ ("true", bool),
    ("false", bool),
    ("zero", funType int bool),
    ("pair", funType a (funType b (pairType a b))),
    ("fst", funType (pairType a b) a),
    ("snd", funType (pairType a b) b),
    ("id", funType a a),
    ("eq", funType a (funType a bool)),
    ("plus", intBinOp),
    ("minus", intBinOp),
    ("times", intBinOp),
    ("div", intBinOp),
    ("and", boolBinOp),
    ("or", boolBinOp),
    ("negate", funType int int),
    ("not", funType bool bool) ]
    where a = TypeVariable "_a"
          b = TypeVariable "_b"
          intBinOp = funType int (funType int int)
          boolBinOp = funType bool (funType bool bool)

runAnalyzer :: Analyzer a -> Either String (a, Env)
runAnalyzer a = case runState (runErrorT (runA a)) basicEnv of
  (Left err, _)  -> Left err
  (Right r, env) -> Right (r, env)

liftA :: State Env a -> Analyzer a
liftA m = Analyzer (lift m)

analyzeExpr :: Expr -> Analyzer Type
analyzeExpr (Num _) = return $ BasicType "int"
analyzeExpr (Var v) = do
  t <- liftA $ retrieveEnv v
  case t of
    Just t' -> return t'
    Nothing -> liftA get >>= \env -> throwError $ "No such variable: " ++ v

analyzeExpr (Cond test ifTrue ifFalse) = do
  t <- analyzeExpr test
  unifyTypes t bool
  t1 <- analyzeExpr ifTrue
  t2 <- analyzeExpr ifFalse
  unifyTypes t1 t2

analyzeExpr (Lambda var body) = saveEnv $ do
  varType <- liftA newVar
  liftA $ addNonGenericVar var varType
  bodyType <- analyzeExpr body
  t <- pruneType $ funType varType bodyType
  return t

analyzeExpr (FunCall func arg) = saveEnv $ do
  funcType <- analyzeExpr func
  argType <- analyzeExpr arg
  resType <- liftA newVar
  unifyTypes funcType (funType argType resType)
  pruneType resType

analyzeExpr (Block decl scope) = do
  analyzeDecl decl
  t <- analyzeExpr scope
  pruneType t

analyzeDecl :: Decl -> Analyzer Type
analyzeDecl (Assign varName expr) = do
  exprType <- analyzeExpr expr
  liftA $ putEnv varName exprType
  return (exprType)

analyzeDecl (Seq a b) = analyzeDecl a >> analyzeDecl b

analyzeDecl (Rec rec) = do
  createBinding rec
  analyzeAndUnify rec


createBinding :: Decl -> Analyzer ()
createBinding (Assign varName expr) = addEnvUnknown varName
createBinding (Seq a b) = createBinding a >> createBinding b
createBinding (Rec rec) = createBinding rec

analyzeAndUnify :: Decl -> Analyzer Type

analyzeAndUnify (Assign varName expr) = do
  Just varType  <- liftA $ retrieveEnv varName
  exprType <- analyzeExpr expr
  unifyTypes varType exprType

analyzeAndUnify (Seq a b) = analyzeAndUnify a >> analyzeAndUnify b
analyzeAndUnify (Rec rec) = analyzeAndUnify rec

saveEnv :: Analyzer a -> Analyzer a
saveEnv analyzer = do
  origEnv <- liftA get
  t <- analyzer
  env <- liftA get
  liftA $ put $ env { vars = vars origEnv, ngvars = ngvars origEnv }
  return t

unifyTypes :: Type -> Type -> Analyzer Type
unifyTypes t1 t2 = do
  env <- liftA get
  let mods = unify t1 t2 (instances env)
  let newInstances = liftM (Map.union (instances env) . Map.fromList) mods
  case newInstances of
    Just ni -> do liftA $ put $ env { instances = ni }
                  return $ prune t1 ni
    Nothing -> throwError $ "Could not unify " ++ (show t1) ++ " and " ++ (show t2) ++ "."

pruneType :: Type -> Analyzer Type
pruneType t = do
  liftA get >>= return . prune t . instances

addEnv :: VarName -> Type -> Analyzer ()
addEnv n t = do
  liftA $ addNonGenericVar n t
  return ()

addEnvUnknown :: VarName -> Analyzer ()
addEnvUnknown n = do
  t <- liftA newVar
  liftA $ addNonGenericVar n t
  return ()
