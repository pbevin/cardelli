module ParseType where

import Control.Applicative hiding (many, optional, (<|>))
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Type

lexeme p = do{ x <- p; whiteSpace; return x  }
whiteSpace = skipMany (satisfy isSpace)
symbol name = lexeme (string name)
parens = between (symbol "(") (symbol ")")
namechar = alphaNum <|> char '_'

parseType :: String -> Type
parseType str = case parse (typedecl <* eof) "" str of
  Left err -> error (show err)
  Right t  -> t

typedecl :: Parser Type
typedecl = function $ lexeme $ concrete <|> abstract <|> list <|> tuple

concrete :: Parser Type
concrete = do
  x <- upper
  xs <- many namechar
  return $ BasicType (x:xs)

abstract :: Parser Type
abstract = do
  x <- lower <|> char '_'
  xs <- many namechar
  return $ TypeVariable (x:xs)

list :: Parser Type
list = between (symbol "[") (symbol "]") typedecl >>= return . listType

tuple :: Parser Type
tuple = do
  types <- parens $ typedecl `sepBy` (symbol ",")
  return $ case types of
    [t] -> t
    otherwise -> TypeOperator "," types

function :: Parser Type -> Parser Type
function p = p `chainr1` do { symbol "->"; return funType }
