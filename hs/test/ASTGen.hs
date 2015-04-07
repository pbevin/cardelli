module ASTGen where

import Control.Applicative
import Test.QuickCheck
import VarName
import AST

instance Arbitrary VarName where
  arbitrary = arbVar `suchThat` (\v -> not(fromVarName v `elem` ["in", "if", "then", "else", "let", "fun"]))

arbVar :: Gen VarName
arbVar = do
    x <- elements $ ['a'..'z']
    xs <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
    return $ VarName (x:xs)

instance Arbitrary Expr where
  arbitrary = sized arbExpr
  shrink = shrinkExpr

instance Arbitrary Decl where
  arbitrary = sized arbDecl
  shrink = shrinkDecl

arbExpr :: Int -> Gen Expr
arbExpr 0 = oneof [ Var <$> fromVarName <$> arbitrary, Num <$> choose(0, 1000) ]
arbExpr n = oneof [ Cond <$> subtree <*> subtree <*> subtree,
                    Lambda <$> fromVarName <$> arbitrary <*> subtree,
                    FunCall <$> subtree <*> subtree,
                    Block <$> arbDecl n <*> subtree ]
               where subtree = arbExpr (n `div` 2)

shrinkExpr (Num _) = [Num 1]
shrinkExpr (Var _) = [Var "a"]
shrinkExpr (Cond a b c) = [a, b, c]
shrinkExpr (Lambda _ a) = [a]
shrinkExpr (FunCall a b) = [a, b]
shrinkExpr (Block d a) = [a] ++ [Block d' a | d' <- shrinkDecl d]

arbDecl :: Int -> Gen Decl
arbDecl 0 = oneof [ Assign <$> fromVarName <$> arbitrary <*> arbExpr 0 ]
arbDecl n = oneof [ Seq <$> subtree <*> subtree,
                    Rec <$> subtree,
                    Assign <$> fromVarName <$> arbitrary <*> arbExpr n ]
  where subtree = arbDecl (n `div` 2)

shrinkDecl (Assign _ e) = [Assign "a" e]
shrinkDecl (Rec d) = [d]
shrinkDecl (Seq a b) = [a, b]
