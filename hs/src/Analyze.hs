{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyze (runAnalyzer, analyzeExpr, initialVars) where

import Control.Monad.State
import Control.Monad.Error
import Control.Applicative (Applicative)
import qualified Data.Map as Map
import Data.Maybe
import AST
import Parse
import Env
import RetrieveEnv
import Type
import Unify

newtype Analyzer a = Analyzer {
  runA :: ErrorT String (State Env) a
} deriving (Monad, MonadError String, Functor, Applicative)

initialVars :: TypeVars
initialVars = emptyVars { vars = defs }
  where defs = Map.fromList
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
        a = TypeVariable "_a"
        b = TypeVariable "_b"
        intBinOp = funType int (funType int int)
        boolBinOp = funType bool (funType bool bool)

runAnalyzer :: Analyzer a -> Either String (a, Env)
runAnalyzer a = case runState (runErrorT (runA a)) emptyEnv of
  (Left err, _)  -> Left err
  (Right r, env) -> Right (r, env)

liftA :: State Env a -> Analyzer a
liftA m = Analyzer (lift m)

analyzeExpr :: TypeVars -> Expr -> Analyzer Type
analyzeExpr tv expr = case expr of
  Num _ -> return $ BasicType "int"
  Var v -> do
    t <- liftA $ retrieveEnv v tv
    case t of
      Just t' -> return t'
      Nothing -> throwError $ "No such variable: " ++ v

  Cond test ifTrue ifFalse -> do
    t <- analyzeExpr tv test
    unifyTypes t bool
    t1 <- analyzeExpr tv ifTrue
    t2 <- analyzeExpr tv ifFalse
    unifyTypes t1 t2

  Lambda var body -> do
    a <- liftA newVar
    b <- analyzeExpr (addNonGeneric a $ putEnv var a tv) body
    t <- pruneType $ funType a b
    return t

  FunCall func arg -> do
    funcType <- analyzeExpr tv func
    argType <- analyzeExpr tv arg
    resType <- liftA newVar
    unifyTypes funcType (funType argType resType)
    pruneType resType

  Block decl scope -> do
    tv' <- analyzeDecl tv decl
    t <- analyzeExpr tv' scope
    pruneType t

analyzeDecl :: TypeVars -> Decl -> Analyzer TypeVars
analyzeDecl tv decl = case decl of
  Assign varName expr -> do
    exprType <- analyzeExpr tv expr
    return $ putEnv varName exprType tv

  Seq a b -> do
    tv' <- analyzeDecl tv a
    analyzeDecl tv' b

  Rec rec -> do
    tv' <- createBinding tv rec
    analyzeAndUnify tv' rec

createBinding :: TypeVars -> Decl -> Analyzer TypeVars
createBinding tv decl = case decl of
  Assign varName _ -> do
    t <- liftA newVar
    return $ putEnv varName t $ addNonGeneric t tv

  Seq a b -> do
    tv' <- createBinding tv a
    createBinding tv' b

  Rec rec -> createBinding tv rec

analyzeAndUnify :: TypeVars -> Decl -> Analyzer TypeVars
analyzeAndUnify tv decl = case decl of
  Assign varName expr -> do
    Just varType  <- liftA $ retrieveEnv varName tv
    exprType <- analyzeExpr tv expr
    unifyTypes varType exprType
    return tv

  Seq a b -> analyzeAndUnify tv a >> analyzeAndUnify tv b

  Rec rec -> analyzeAndUnify tv rec

unifyTypes :: Type -> Type -> Analyzer Type
unifyTypes t1 t2 = do
  env <- liftA get
  let mods = unify t1 t2 (instances env)
  let newInstances = liftM (Map.union (instances env) . Map.fromList) mods
  case newInstances of
    Just ni -> do liftA $ put $ env { instances = ni }
                  return $ prune ni t1
    Nothing -> throwError $ "Could not unify " ++ (show t1) ++ " and " ++ (show t2) ++ "."

pruneType :: Type -> Analyzer Type
pruneType t = liftA get >>= return . (flip prune) t . instances
