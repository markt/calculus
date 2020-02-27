import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.LeanCheck as LC
import Text.Megaparsec


import Expressions
import Printing


main :: IO ()

main = defaultMain (testGroup "Library Tests" [test1]) --test3, test4, test5, test6, test7, test8, test9, test10])
test1 :: TestTree--, test2, test3, test4, test5, test6, test7, test8, test9, test10 :: TestTree


{-
    Correctness tests: 5
-}

exampleVar :: Expr
exampleVar = Var "f1"

--exampleLaw :: Law
--exampleLaw = Law "zero" (Compose [Con "+" [Compose [Var "x"], Compose [Val 0]]], Compose [Var "x"])



parseG :: String -> Expr
parseG str = case runParser expr "" str of
    Left er -> Var []
    Right e -> e


parseL :: String -> Law
parseL str = case runParser law "" str of
    Left er -> Law [] (Var [], Var [])
    Right e -> e

test1 =
  LC.testProperty "var test" (exampleVar == (parseG "f1"))
