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
import Data.Maybe


type Parser = Parsec Void String


-- Datatypes

-- newtype Expr = Compose [Atom] deriving Eq
data Expr
    = Var String
    | Val Int
    | Con String [Expr] deriving (Show, Eq)
-- TODO: add deriv type: Deriv String [Expr]

-- data Law = Law String Equation deriving (Show, Eq)
-- type Equation = (Expr, Expr)


data Law = Law LawName Equation deriving (Show, Eq)
type LawName = String
type Equation = (Expr, Expr)


data Calculation = Calc Expr [Step]
data Step = Step LawName Expr
-- data Step = Step String Expr


type Subst = [(Expr,Expr)]
-- type VarName = String

emptySub = []
unitSub v e = [(v,e)]

-- unitSub :: Expr -> Expr -> Subst
-- unitSub v e = [(v,e)]





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

expr :: Parser Expr
expr = makeExprParser term operatorTable <?> "expression"

atom :: Parser String
atom = ((:) <$> letterChar <*> many alphaNumChar) <* space



-- TODO add function application support e.g. sin(x)
-- consider only apply for unary, makeexprparser for binary
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "sin" (func "sin")]
  , [ binary "^" (funcc "^") ]
  , [ binary  "*" (funcc "*")          --[ InfixL (prod <$ symbol "*")
    , binary  "/"  (funcc "/")  ]
  , [ binary  "+"  (funcc "+")
    , binary  "-"  (funcc "-") ]
  ]


func::String -> Expr -> Expr
func name a = Con name [a]


funcc::String -> Expr -> Expr -> Expr
funcc name a b = Con name [a,b]



binary  name f = InfixL  (f <$ symbol name)
prefix  name f = Prefix  (f <$ symbol name)

law :: Parser Law
law = do {name <- upto ':'; space; e1 <- expr; space; char '='; space; e2 <- expr; space; return (Law name (e1,e2))}


upto :: Char -> Parser String
upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c)

splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (a:as) = [([],a:as)] ++ [(a:as1,as2) | (as1,as2) <- splitsH as]
splitsH as = [(take n as,drop n as) | n <- [0..length as]]

splitsN :: Int -> [a] -> [[[a]]]
splitsN 0 [] = [[]]
splitsN 0 as = []
splitsN n as = [bs : bss | (bs,cs) <- splits as, bss <- splitsN (n-1) cs]


splitsAll :: [a] -> [[[a]]]
splitsAll as = splitsN (length as) as




match :: (Expr,Expr) -> [Subst]
match (Var v,e) = [unitSub (Var v) e]



apply :: Subst -> Expr -> Expr
apply sub e = binding sub e


binding :: Subst -> Expr -> Expr
binding sub e = fromJust (lookup e sub)




