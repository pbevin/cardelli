module ShowExpr (showExpr) where

import AST
import Debug.Trace

showExpr :: Expr -> String
showExpr expr = case expr of
  Var a -> a
  Num n -> show n
  Cond test ifTrue ifFalse ->
    "if " ++ (showExpr test) ++
    " then " ++ (showExpr ifTrue) ++
    " else " ++ (showExpr ifFalse)
  FunCall f x -> parens (showExpr f) ++ parens(showExpr x)
  Lambda var e -> "fun " ++ parens var ++ " " ++ parens (showExpr e)
  Block d e -> "let " ++ (showDecl d) ++ " in " ++ (showExpr e)

showDecl decl = case decl of
  Assign var e -> var ++ " = " ++ (showExpr e)
  Seq a b -> parens (showDecl a) ++ " then " ++ parens (showDecl b)
  Rec d -> "rec " ++ (showDecl d)


parens str = "(" ++ str ++ ")"
