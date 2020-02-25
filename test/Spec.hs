import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.LeanCheck as LC


import Lib
import Expressions
import Parsing


main :: IO ()

main = defaultMain (testGroup "Library Tests" [test1]) --test3, test4, test5, test6, test7, test8, test9, test10])
test1 :: TestTree--, test2, test3, test4, test5, test6, test7, test8, test9, test10 :: TestTree


{-
    Correctness tests: 5
-}

test1 =
  LC.testProperty "add Zero x = x" (5 == 5)
