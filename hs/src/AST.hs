module AST where

type Ident = String

data Expr = Var Ident
          | Num Integer
          | Cond Expr Expr Expr
          | Lambda Ident Expr
          | FunCall Expr Expr
          | Block Decl Expr
          deriving (Show, Eq)

data Decl = Assign Ident Expr
          | Seq Decl Decl
          | Rec Decl
          deriving (Show, Eq)

