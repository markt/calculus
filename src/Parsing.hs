module Parsing where

import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Parsing

type Parser = Parsec Void String

-- parse white space and comments
sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)


-- parse token
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- parse symbols
symbol :: String -> Parser String
symbol = L.symbol sc

-- parse any string between parenthesis
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


-- parse variable
var :: Parser String
var = ((:) <$> letterChar <*> many alphaNumChar) <* space

-- parser that parses any string until char c
upto :: Char -> Parser String
upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c)

