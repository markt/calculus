import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.LeanCheck as LC
import Text.Megaparsec


import Expressions
import Printing
import System.IO
import Control.Monad  


-- readLaws :: IO ([Law])
-- readLaws = do
--   contents <- readFile "laws.txt"
--   let lawStrings = lines contents
--   let laws = map (parseL) lawStrings
--   return laws

-- extractLaws :: [Law]
-- extractLaws = do
-- 	lawss <- readLaws
-- 	return lawss

-- main = defaultMain (testGroup "Library Tests" [varTest,valTest,conTest,funTest,lawTest,lawsTest]) --test3, test4, test5, test6, test7, test8, test9, test10])
--varTest :: TestTree--, test2, test3, test4, test5, test6, test7, test8, test9, test10 :: TestTree


--{-
--    Correctness tests: 5
---}

--exampleVar :: Expr
--exampleVar = Var "f1"

--exampleVal :: Expr
--exampleVal = Val 1

--exampleCon :: Expr
--exampleCon = Con "*" [Var "f", Val 1]

--exampleFun :: Expr
--exampleFun = Con "sin" [Var "x"]

--exampleLaw :: Law
--exampleLaw = Law "zeros" (Con "+" [Var "x",Val 0],Var "x")



--exampleExpr :: Expr
--exampleExpr = Con "+" [Con "+" [Var "x", Val 0], Var "y"]

---- exampleSubst :: Subst
---- exampleSubst = Subst [(Var "x",(Con "+" [Var "x", Val 0]))]

--lawList :: [Law]
--lawList = extractLaws (readLaws "laws.txt")



--parseG :: String -> Expr
--parseG str = case runParser expr "" str of
--    Left er -> Var []
--    Right e -> e


--parseL :: String -> Law
--parseL str = case runParser law "" str of
--   Left er -> Law [] (Var [], Var [])
--   Right e -> e

--varTest =
--  LC.testProperty "var test" (exampleVar == (parseG "f1"))

--valTest =
--  LC.testProperty "val test" (exampleVal == (parseG "1"))

--conTest =
--  LC.testProperty "con test" (exampleCon == (parseG "(f * 1)"))

--funTest =
--  LC.testProperty "fun test" (exampleFun == (parseG "sin x"))

--lawTest =
--  LC.testProperty "law test" (exampleLaw == (parseL "zeros: x + 0 = x"))

--lawsTest =
--  LC.testProperty "laws test" (lawList == [exampleLaw])

---- lawNameTest =
----   LC.testProperty "printing lawName test" ((showLaw exampleLaw) == (showString "zeros"))
