{-# LANGUAGE OverloadedStrings #-}
module Printing where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.List (intersperse)
import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Expressions


showCalculation :: Calculation -> ShowS
showCalculation (Calc e steps)
    = showString "\n\n  " .
      (showExpr e) .
      showString "\n\n" .
      compose (map showStep steps)

showStep :: Step -> ShowS
showStep (Step law e)
    = showString "= {" .
      showString law .
      showString "}\n\n  " .
      (showExpr e) .
      showString "\n\n"



showExpr :: Expr -> ShowS
showExpr (Var v) = showString v
showExpr (Val v) = showString (show v)
showExpr (Con f []) = showString f
showExpr (Con f [e]) = showString f . showSpace . showExpr e
showExpr (Con f [e1,e2]) | isOp f
 = showParen True (showExpr e1 . showSpace . showString f . showSpace . showExpr e2)
showExpr (Deriv v e) = showString "d/d" . showString v . showSpace . showExpr e

showSep :: String -> (a -> ShowS) -> [a] -> ShowS
showSep sep f = compose . intersperse (showString sep) . map f

showSpace :: ShowS
showSpace = (' ':)

compose :: [b -> b] -> b -> b
compose = foldr (.) id

isOp :: [Char] -> Bool
isOp = not . all isAlphaNum


parseExpr :: String -> Expr
parseExpr str = case parse expr "" str of
    Left er -> Var "error"
    Right e -> e




showLaw :: Law -> ShowS
showLaw (Law s (e1,e2)) = showString s

parseL :: String -> Law
parseL str = case runParser law "" str of
    Left er -> Law [] (Var [], Var [])
    Right e -> e

