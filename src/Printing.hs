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
