{-# LANGUAGE OverloadedStrings #-}
module Expressions where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.List (intersperse)
import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String


-- Datatypes

newtype Expr = Compose [Atom] deriving Eq
data Atom
    = Var String
    | Val Int
    | Con String [Expr] deriving Eq

data Law = Law LawName Equation
type LawName = String
type Equation = (Expr, Expr)


data Calculation = Calc Expr [Step]
data Step = Step LawName Expr






-- Parsing

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens x = string "(" *> x <* string ")" <* space
-- parens = between (symbol "(") (symbol ")")

term :: Parser Expr
term = ("id" *> pure (Compose []))
       <|> parens expr
       <|> (atom >>= more) <?> "term"
       where 
        more v@(_:d:_)
           | isDigit d = return (Compose [Var v])
           | otherwise = do {ts <- many (expr <* space);return (Compose [(Con v ts)])}
        more v = return (Compose [Var v])

operatorE = do opr <- operator
               return (\x y -> Compose [Con opr [x,y]])

composeE (Compose as) (Compose bs) = Compose (as ++ bs)

-- applicationE f e = Compose [Con f [e]]
-- applicationE f e = do {f <- function; e <- expr; return (Compose [Con f [e]])}

function :: Parser String
function = sin <|> cos
  where
    sin = string "sin"
    cos = string "cos"

expr :: Parser Expr
expr = makeExprParser term operatorTable <?> "expression"

atom :: Parser String
atom = ((:) <$> letterChar <*> many alphaNumChar) <* space

operator :: Parser String
operator = try (do op <- some (satisfy symbolic) <* space
                   guard (op /= "." && op /= "=")
                   return op)
    where symbolic = (`elem` (opsymbols::[Char]))
          opsymbols = "!@#$%^&*()_+-={}[]:\"|;'\\,./<>?`~±§"

-- TODO add function application support e.g. sin(x)
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL ("." *> space *> pure composeE) ]
  , [ InfixL operatorE ]
  ]


law :: Parser Law
law = do {name <- upto ':'; e1 <- expr; e2 <- expr; return (Law name (e1,e2))}

upto :: Char -> Parser String
upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c)





