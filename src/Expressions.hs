{-# LANGUAGE OverloadedStrings #-}
module Expressions where

-- import Parsing
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Text (Text, intersperse)
import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text


-- Datatypes

newtype Expr = Compose [Atom] deriving Eq
data Atom
    = Var String
    | Con String [Expr] deriving Eq

data Law = Law LawName Equation
type LawName = String
type Equation = (Expr, Expr)


data Calculation = Calc Expr [Step]
data Step = Step LawName Expr







-- Printing

showExpr :: Int -> Expr -> ShowS
showExpr _ (Compose []) = showString "id"
showExpr p (Compose [a]) = showAtom p a
showExpr p (Compose as)
 = showParen (p>0)
    (showSep " . " (showAtom 1) as)

showAtom :: Int -> Atom -> ShowS
showAtom _ (Var v) = showString v
showAtom _ (Con f []) = showString f
showAtom p (Con f [e1,e2]) | isOp f
 = showParen (p>0) (showExpr 1 e1 . showSpace . showString f . showSpace . showExpr 1 e2)
showAtom p (Con f es)
 = showParen (p>1)
    (showString f . showSpace . showSep " " (showExpr 2) es)

showSep :: String -> (a -> ShowS) -> [a] -> ShowS
showSep sep f = compose . intersperse (showString sep) . map f

showSpace :: ShowS
showSpace = (' ':)

compose :: [b -> b] -> b -> b
compose = foldr (.) id

isOp :: [Char] -> Bool
isOp = not . all isAlphaNum


parseE :: String -> IO ()
parseE str = case parse expr "" str of
    Left er -> putStr (errorBundlePretty er)
    Right e -> putStrLn (showExpr 0 e "")




-- Parsing

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

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

expr :: Parser Expr
expr = makeExprParser term operatorTable <?> "expression"

atom :: Parser String
atom = ((:) <$> letterChar <*> some alphaNumChar) <* space

operator :: Parser String
operator = try (do op <- some (satisfy symbolic) <* space
                   guard (op /= "." && op /= "=")
                   return op)
    where symbolic = (`elem` (opsymbols::[Char]))
          opsymbols = "!@#$%^&*()_+-={}[]:\"|;'\\,./<>?`~±§"

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL ("." *> space *> pure composeE) ]
  , [ InfixL operatorE ]