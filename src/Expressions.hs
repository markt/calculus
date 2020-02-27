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

-- newtype Expr = Compose [Atom] deriving Eq
data Expr
    = Var String
    | Val Int
    | Con String [Expr] deriving Eq
-- TODO: add deriv type: Deriv String [Expr]

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
parens = between (symbol "(") (symbol ")")
-- parens x = string "(" *> x <* string ")" <* space


term :: Parser Expr
term = parens expr
       <|> pInteger
       <|> (atom >>= more)
       where
        more v@(_:d:_)
          | isDigit d = return (Var v)
          | otherwise = do {ts <- many (expr <* space); return (Con v ts)}
        more v = return (Var v)

pInteger :: Parser Expr
pInteger = Val <$> lexeme L.decimal

-- term :: Parser Expr
-- term = ("id" *> pure (Compose []))
--        <|> parens expr
--        <|> (atom >>= more) <?> "term"
--        where 
--         more v@(_:d:_)
--            | isDigit d = return (Compose [Var v])
--            | otherwise = do {ts <- many (expr <* space);return (Compose [(Con v ts)])}
--         more v = return (Compose [Var v])

-- operatorE = do opr <- operator
--                return (\x y -> Compose [Con opr [x,y]])

-- composeE (Compose as) (Compose bs) = Compose (as ++ bs)

-- applicationE f e = Compose [Con f [e]]
-- applicationE f e = do {f <- function; e <- expr; return (Compose [Con f [e]])}

-- function :: Parser String
-- function = sin <|> cos
--   where
--     sin = string "sin"
--     cos = string "cos"

expr :: Parser Expr
expr = makeExprParser term operatorTable <?> "expression"

atom :: Parser String
atom = ((:) <$> letterChar <*> many alphaNumChar) <* space

-- operator :: Parser String
-- operator = try (do op <- some (satisfy symbolic) <* space
--                    guard (op /= "." && op /= "=")
--                    return op)
--     where symbolic = (`elem` (opsymbols::[Char]))
--           opsymbols = "!@#$%^&*_+-=:\"|;'\\,./<>?`~±§"

-- TODO add function application support e.g. sin(x)
-- consider only apply for unary, makeexprparser for binary
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "sin" sinF]
  , [ binary "^" pow ]
  , [ binary  "*" prod          --[ InfixL (prod <$ symbol "*")
    , binary  "/"  divi  ]
  , [ binary  "+"  add
    , binary  "-"  sub ]
  ]

sinF a = Con "sin" [x]
  where
    x = case a of
          (Var a) -> (Var a)
          (Val a) -> (Val a)
          (Con a as) -> (Con a as)

pow a b = Con "^" [x,y]
  where
    x = case a of
          (Var a) -> (Var a)
          (Val a) -> (Val a)
          (Con a as) -> (Con a as)
    y = case b of
          (Var b) -> (Var b)
          (Val b) -> (Val b)
          (Con b bs) -> (Con b bs)

prod a b = Con "*" [x,y]
  where
    x = case a of
          (Var a) -> (Var a)
          (Val a) -> (Val a)
          (Con a as) -> (Con a as)
    y = case b of
          (Var b) -> (Var b)
          (Val b) -> (Val b)
          (Con b bs) -> (Con b bs)

divi a b = Con "/" [x,y]
  where
    x = case a of
          (Var a) -> (Var a)
          (Val a) -> (Val a)
          (Con a as) -> (Con a as)
    y = case b of
          (Var b) -> (Var b)
          (Val b) -> (Val b)
          (Con b bs) -> (Con b bs)

add a b = Con "+" [x,y]
  where
    x = case a of
          (Var a) -> (Var a)
          (Val a) -> (Val a)
          (Con a as) -> (Con a as)
    y = case b of
          (Var b) -> (Var b)
          (Val b) -> (Val b)
          (Con b bs) -> (Con b bs)

sub a b = Con "-" [x,y]
  where
    x = case a of
          (Var a) -> (Var a)
          (Val a) -> (Val a)
          (Con a as) -> (Con a as)
    y = case b of
          (Var b) -> (Var b)
          (Val b) -> (Val b)
          (Con b bs) -> (Con b bs)

-- prod a (Var b) = case a of
--                       (Var a) -> Con "*" [(Var a),(Var b)]
--                       (Val a) -> Con "*" [(Val a),(Var b)]

-- prod a (Var b) = Con "*" [a,(Var b)]
--                  where a = (Var a)

-- prod (Var a) (Var b) = Con "*" [(Var a),(Var b)]
-- prod (Val a) (Val b) = Val (a * b)
-- prod (Con a as) (Con b bs) = Con "*" [(Con a as),(Con b bs)]

-- divi (Var a) (Var b) = Con "/" [(Var a),(Var b)]
-- add (Var a) (Var b) = Con "+" [(Var a),(Var b)]
-- sub (Var a) (Var b) = Con "-" [(Var a),(Var b)]

-- prod (Var a) (Var b) = Con "*" [(Var a),(Var b)]
-- divi (Var a) (Var b) = Con "/" [(Var a),(Var b)]
-- add (Var a) (Var b) = Con "+" [(Var a),(Var b)]
-- sub (Var a) (Var b) = Con "-" [(Var a),(Var b)]

-- prod = do f <- symbol
--        return (\x y -> Con f [x,y])

binary  name f = InfixL  (f <$ symbol name)
prefix  name f = Prefix  (f <$ symbol name)

law :: Parser Law
law = do {name <- upto ':'; e1 <- expr; e2 <- expr; return (Law name (e1,e2))}

upto :: Char -> Parser String
upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c)





