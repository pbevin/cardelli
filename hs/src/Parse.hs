module Parse(parseExpr, parseFun) where

import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)

import AST

funStyle :: T.LanguageDef st
funStyle = emptyDef{ T.commentStart = "{-"
                   , T.commentEnd = "-}"
                   , T.identStart = letter
                   , T.identLetter = alphaNum
                   , T.reservedOpNames = ["="]
                   , T.reservedNames = ["fun", "let", "rec", "in",
                                        "if", "then", "else"]
                   }

-- Exp ::=
--   Ide |
--   "if" Exp "then" Exp "else" Exp |
--   "fun" "(" Ide ")" Exp |
--   Exp "(" Exp ")" |
--   "let" Decl "in" Exp | "(" Exp ")"
-- Decl ::=
--   Ide "=" Exp |
--   Decl "then" Decl | "rec" Decl |
--   "(" Decl ")"

-- http://stackoverflow.com/a/26539615/183140
lexer = T.makeTokenParser funStyle
parens = T.parens lexer
identifier = T.identifier lexer
integer = T.integer lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
symbol = T.symbol lexer

atom = liftM Var identifier
number = liftM Num integer

cond :: Parser Expr
cond = do
  reserved "if"
  test <- expr
  reserved "then"
  ifTrue <- expr
  reserved "else"
  ifFalse <- expr
  return $ Cond test ifTrue ifFalse

lambda :: Parser Expr
lambda = do
  reserved "fun"
  var <- parens identifier
  body <- expr
  return $ Lambda var body

block :: Parser Expr
block = do
  reserved "let"
  d <- decl
  reserved "in"
  e <- expr
  return $ Block d e

term :: Parser Expr
term = let simpleTerm = parens(expr)
                    <|> cond
                    <|> block
                    <|> lambda
                    <|> number
                    <|> atom
       in do
         t <- simpleTerm
         applications <- many (parens exprList)
         return $ foldl FunCall t $ concat applications

exprList :: Parser [Expr]
exprList = expr `sepBy` (symbol ",")

mkUnOp op a = FunCall (Var op) a
mkBinOp op a b = FunCall (mkUnOp op a) b

binary name fn = Infix (do { reservedOp name; return (mkBinOp fn) }) AssocLeft
unary name fn = Prefix (do { reservedOp name; return (mkUnOp fn) })

table = [
  [ unary "-" "negate" ],
  [ unary "!" "not" ],
  [ binary "&" "and", binary "|" "or" ],
  [ binary "*" "times", binary "/" "div" ],
  [ binary "+" "plus", binary "-" "minus" ],
  [ binary "==" "eq" ]]

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"

assign :: Parser Decl
assign = do
  var <- identifier
  reservedOp "="
  e <- expr
  return $ Assign var e

seqDecl :: Parser Decl
seqDecl = liftM2 Seq decl decl

rec :: Parser Decl
rec = liftM Rec (reserved "rec" *> decl)

decl :: Parser Decl
decl = let simpleDecl = assign <|> rec <|> parens(decl)
       in do
         t <- simpleDecl `sepBy` (reserved "then")
         return $ foldl1 Seq t

parseExpr :: String -> Either ParseError Expr
parseExpr str = parse (expr <* eof) "" str

parseFun :: String -> Expr
parseFun str = case parseExpr str of
                Left err -> error $ show err
                Right expr -> expr
