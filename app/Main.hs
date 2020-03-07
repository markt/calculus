module Main where

import Expressions
import Printing
import System.IO
import Control.Monad  
import System.Random
import Data.Map

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

-- Random shuffler of lists
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ Prelude.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)

-- returns number of steps within Calculation
lenOfCalc :: Calculation -> Int
lenOfCalc (Calc e steps) = length steps


-- calculates steps N times, each time shuffling the list of laws
sample:: RandomGen g =>  Int -> [Law] -> Expr -> g -> [(Int, Calculation)]
sample 0 _ _ _ = []
sample n laws e gen = (lenOfCalc x, x):(sample (n-1) laws e (snd res))
       where x = calculate (fst $ res) e
             res = fisherYates gen laws

-- returns a min value tuple, where t is the value used for ordering
minimum' :: Ord t => [(t, a)] -> (t, a)
minimum' []     = error "minimum of empty list"
minimum' (x:xs) = minTail x xs
  where minTail currentMin [] = currentMin
        minTail (m, n) (p:ps)
          | m > (fst p) = minTail p ps
          | otherwise   = minTail (m, n) ps



-- read laws from a location specified
-- then loop by accepting user expression and applying the laws to these expressions
readLaws :: FilePath -> IO ()
readLaws ff = do
  contents <- readFile ff
  let lawStrings = lines contents
  
  putStrLn ("\n\n\nWelcome to our calculus calculator!\n\n\nEnter an expression to be solved:\n")

  forever $ do
    x <- getLine
    let e = parseExpr x
    let laws = Prelude.map (parseL) (lawStrings)
    let orig_order_calc = calculate laws e
    let calculations = ((lenOfCalc orig_order_calc, orig_order_calc):(sample 5 laws e (mkStdGen 0)))
    let lengths_of_steps = Prelude.map (show . fst) calculations
    let str = "\nThe lengths of each respective shuffled law calculation (first calculation uses unshuffled order): " ++ head lengths_of_steps  ++ (Prelude.foldl (++) "" $ Prelude.map (", " ++ ) $ tail lengths_of_steps)
    putStrLn str
    putStrLn ((showCalculation (snd $ minimum' calculations)) "")
    putStrLn ("\n\n\nEnter another expression to be solved:\n")


  return ()


main :: IO ()
main = readLaws "../calculus/laws.txt"
