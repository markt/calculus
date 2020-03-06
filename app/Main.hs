module Main where

import Expressions
import Printing
import System.IO
import Control.Monad  


-- read laws from a location specified
-- then loop by accepting user expression and applying the laws to these expressions
readLaws :: FilePath -> IO ()
readLaws ff = do
  contents <- readFile ff
  let lawStrings = lines contents
  let laws = map (parseL) lawStrings
  putStrLn ("\n\n\nWelcome to our calculus calculator!\n\nEnter an expression to be solved:")

  forever $ do
    x <- getLine
    let e = parseExpr x
    let calc = calculate laws e
    putStrLn (show calc)
  return ()


main :: IO ()
main = readLaws "../calculus/laws.txt"
