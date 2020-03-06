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



instance Show Expr where
 show (Var s) = s
 show (Val i) = show i
 show (Con v es) = " (" ++ show (head es) ++ (foldl (++) "" (map (((" " ++ v ++ " ") ++) . show) (tail es))) ++ ")"
 show (Deriv v e) = "d/d" ++ show v ++ " " ++ show e


instance Show Step where
 show (Step l e) = "=   {" ++ l ++ "}\n" ++ show e ++ "\n"

instance Show Calculation where
 show (Calc e steps) = "\n" ++ show e ++ "\n" ++ (foldl (++) "" (map show steps))


instance Show Law where
  show (Law ln eq) = "Law " ++ ln  ++ " " ++ show eq 



-- showCalculation :: Calculation -> ShowS
-- showCalculation (Calc e steps)
--     = showString "\n " .
--       (showExpr e) .
--       showChar '\n' .
--       compose (map showStep steps)
-- -- compose = foldr (.) id

-- showStep :: Step -> ShowS
-- showStep (Step law e)
--     = showString "=   {" .
--       showString law .
--       showString "}\n " .
--       (showExpr e) .
--       showChar '\n'



-- showExpr :: Expr -> ShowS
-- showExpr (Var v) = showString v
-- showExpr (Val v) = showString (show v)
-- showExpr (Con f []) = showString f
-- showExpr (Con f [e1,e2]) | isOp f
--  = showParen True (showExpr e1 . showSpace . showString f . showSpace . showExpr e2)
-- showExpr (Con f es)
--  = showParen True
--     (showString f . showSpace . showSep " " (showExpr) es)





-- showExpr :: Int -> Expr -> ShowS
-- showExpr _ (Var v) = showString v
-- showExpr _ (Val v) = showString (show v)
-- showExpr _ (Con f []) = showString f
-- showExpr p (Con f [e1,e2]) | isOp f
--  = showParen (p>0) (showExpr 1 e1 . showSpace . showString f . showSpace . showExpr 1 e2)
-- showExpr p (Con f es)
--  = showParen (p>1)
--     (showString f . showSpace . showSep " " (showExpr 2) es)

-- showExpr :: Int -> Expr -> ShowS
-- showExpr _ (Compose []) = showString "id"
-- showExpr p (Compose [a]) = showAtom p a
-- showExpr p (Compose as)
--  = showParen (p>0)
--     (showSep " . " (showAtom 1) as)

-- showAtom :: Int -> Atom -> ShowS
-- showAtom _ (Var v) = showString v
-- showAtom _ (Con f []) = showString f
-- showAtom p (Con f [e1,e2]) | isOp f
--  = showParen (p>0) (showExpr 1 e1 . showSpace . showString f . showSpace . showExpr 1 e2)
-- showAtom p (Con f es)
--  = showParen (p>1)
--     (showString f . showSpace . showSep " " (showExpr 2) es)

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


-- parseE :: String -> IO ()
-- parseE str = case parse expr "" str of
--     Left er -> putStr (errorBundlePretty er)
--     Right e -> putStrLn (showExpr e "")

-- parseE :: String -> IO ()
-- parseE str = case parse expr "" str of
--     Left er -> putStr (errorBundlePretty er)
--     Right e -> putStrLn (showExpr 0 e "")


showLaw :: Law -> ShowS
showLaw (Law s (e1,e2)) = showString s

parseL :: String -> Law
parseL str = case runParser law "" str of
    Left er -> Law [] (Var [], Var [])
    Right e -> e

-- basically just used to demonstrate law printing
parseLN :: String -> IO ()
parseLN str = case runParser law "" str of
    Left er -> putStrLn (errorBundlePretty er)
    Right e -> putStrLn (showLaw e "")

