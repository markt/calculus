module Main where

import Expressions
import Printing
import System.IO
import Control.Monad  



readLaws :: FilePath -> IO ()
readLaws ff = do
  contents <- readFile ff
  let lawStrings = lines contents
  let laws = map (parseL) lawStrings
  putStrLn ("\n" ++ show laws)

  forever $ do
    x <- getLine
    let e = parseExpr x
    let calc = calculate laws e
    -- putStrLn (showCalculation calc)
    putStrLn (show calc)
  return ()


--main = forever $ do  
--    putStr "Give me some input: "  
--    l <- getLine  
--    putStrLn $ map toUpper l  


x = readLaws "/Users/marktaylor/cs69/calculus/laws.txt"

main :: IO ()
main = readLaws "/Users/marktaylor/cs69/calculus/laws.txt"
