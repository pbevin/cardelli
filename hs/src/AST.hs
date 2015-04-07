module AST where

data Expr = Var String
          | Num Integer
          | Cond Expr Expr Expr
          | Lambda String Expr
          | FunCall Expr Expr
          | Block Decl Expr
          deriving (Show, Eq)

data Decl = Assign String Expr
          | Seq Decl Decl
          | Rec Decl
          deriving (Show, Eq)

