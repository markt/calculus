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
    putStrLn x
  return ()


--main = forever $ do  
--    putStr "Give me some input: "  
--    l <- getLine  
--    putStrLn $ map toUpper l  


x = readLaws "/Users/rednecked_crake/laws"

main :: IO ()

main = readLaws "/Users/rednecked_crake/laws"
