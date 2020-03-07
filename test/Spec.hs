import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.LeanCheck as LC
import Text.Megaparsec


import Expressions
import Printing
import System.IO
import Control.Monad  


main = defaultMain (testGroup "Library Tests" [varTest,valTest,conTest,funTest,lawTest,lawNameTest, matchTest, unifyTest])
varTest :: TestTree


{-
   Correctness tests: 5
-}

exampleVar :: Expr
exampleVar = Var "f1"

exampleVal :: Expr
exampleVal = Val 1

exampleCon :: Expr
exampleCon = Con "*" [Var "f", Val 1]

exampleFun :: Expr
exampleFun = Con "sin" [Var "x"]

exampleLaw :: Law
exampleLaw = Law "zeros" (Con "+" [Var "x",Val 0],Var "x")


exampleExpr :: Expr
exampleExpr = Con "+" [Con "+" [Var "x", Val 0], Var "y"]


lawList :: [Law]
lawList = [Law "identity add" (Con "+" [Var "p",Var "q"],Con "+" [Var "p",Var "q"]),Law "zero power" (Con "^" [Var "x",Val 0],Val 1),Law "one power" (Con "^" [Var "x",Val 1],Var "x"),Law "const on left add" (Con "+" [Var "x",Var "p"],Con "+" [Var "p",Var "x"]),Law "const on left prod" (Con "*" [Var "x",Var "p"],Con "*" [Var "p",Var "x"]),Law "zeros add" (Con "+" [Val 0,Var "x"],Var "x"),Law "zeros sub" (Con "-" [Var "x",Val 0],Var "x"),Law "factor" (Con "*" [Var "p",Con "*" [Var "q",Var "x"]],Con "*" [Con "*" [Var "p",Var "q"],Var "x"]),Law "cancel" (Con "-" [Var "x",Var "x"],Val 0),Law "zeros mult" (Con "*" [Val 0,Var "x"],Val 0),Law "one mult" (Con "*" [Val 1,Var "x"],Var "x"),Law "exp add" (Con "^" [Var "a",Con "+" [Var "b",Var "c"]],Con "*" [Con "^" [Var "a",Var "b"],Con "^" [Var "a",Var "c"]]),Law "exp rearrange" (Con "^" [Con "^" [Var "x",Var "y"],Var "z"],Con "^" [Var "x",Con "*" [Var "y",Var "z"]]),Law "square" (Con "^" [Con "+" [Var "x",Var "y"],Val 2],Con "+" [Con "+" [Con "^" [Var "x",Val 2],Con "*" [Con "*" [Val 2,Var "x"],Var "y"]],Con "^" [Var "y",Val 2]]),Law "square const" (Con "^" [Con "+" [Var "p",Var "x"],Val 2],Con "+" [Con "+" [Con "^" [Var "x",Val 2],Con "*" [Con "*" [Val 2,Var "p"],Var "x"]],Con "^" [Var "p",Val 2]]),Law "const" (Deriv "x" (Var "p"),Val 0),Law "simp" (Deriv "x" (Con "*" [Var "p",Var "x"]),Var "p"),Law "frac deriv" (Deriv "x" (Con "/" [Var "p",Var "q"]),Val 0),Law "power" (Deriv "x" (Con "^" [Var "x",Var "p"]),Con "*" [Var "p",Con "^" [Var "x",Con "-" [Var "p",Val 1]]]),Law "addition" (Deriv "x" (Con "+" [Var "a",Var "b"]),Con "+" [Deriv "x" (Var "a"),Deriv "x" (Var "b")]),Law "subtraction" (Deriv "x" (Con "-" [Var "a",Var "b"]),Con "-" [Deriv "x" (Var "a"),Deriv "x" (Var "b")]),Law "product rule" (Deriv "x" (Con "*" [Var "a",Var "b"]),Con "+" [Con "*" [Deriv "x" (Var "a"),Var "b"],Con "*" [Var "a",Deriv "x" (Var "b")]]),Law "quotient rule" (Deriv "x" (Con "/" [Var "a",Var "b"]),Con "/" [Deriv "x" (Con "-" [Con "*" [Var "a",Var "b"],Con "*" [Var "a",Deriv "x" (Var "b")]]),Con "^" [Var "b",Val 2]]),Law "square deriv" (Deriv "x" (Con "^" [Con "+" [Var "x",Var "y"],Val 2]),Deriv "x" (Con "+" [Con "+" [Con "^" [Var "x",Val 2],Con "*" [Con "*" [Val 2,Var "x"],Var "y"]],Con "^" [Var "y",Val 2]])),Law "square const deriv" (Deriv "x" (Con "^" [Con "+" [Var "p",Var "x"],Val 2]),Deriv "x" (Con "+" [Con "+" [Con "^" [Var "x",Val 2],Con "*" [Con "*" [Val 2,Var "p"],Var "x"]],Con "^" [Var "p",Val 2]])),Law "sin" (Deriv "x" (Con "sin" [Var "a"]),Con "*" [Con "cos" [Var "a"],Deriv "x" (Var "a")]),Law "cos" (Deriv "x" (Con "cos" [Var "a"]),Con "*" [Con "-" [Val 0,Con "sin" [Var "a"]],Deriv "x" (Var "a")]),Law "ln" (Deriv "x" (Con "ln" [Var "a"]),Con "*" [Con "/" [Val 1,Var "a"],Deriv "x" (Var "a")]),Law "basic" (Deriv "x" (Var "x"),Val 1)]

exampleCalcExpr :: Expr
exampleCalcExpr = Deriv "x" (Con "sin" [Con "^" [Var "x", Val 2]])

exampleCalc :: Calculation
exampleCalc = Calc (Deriv "x" (Con "sin" [Con "^" [Var "x",Val 2]])) [(Step "sin" (Con "*" [Con "cos" [Con "^" [Var "x",Val 2]],Deriv "x" (Con "^" [Var "x",Val 2])])), (Step "power" (Con "*" [Con "cos" [Con "^" [Var "x",Val 2]],Con "*" [Val 2,Con "^" [Var "x",Con "-" [Val 2,Val 1]]]]))]



parseG :: String -> Expr
parseG str = case runParser expr "" str of
   Left er -> Var []
   Right e -> e


parseL :: String -> Law
parseL str = case runParser law "" str of
  Left er -> Law [] (Var [], Var [])
  Right e -> e

varTest =
 LC.testProperty "var test" (exampleVar == (parseG "f1"))

valTest =
 LC.testProperty "val test" (exampleVal == (parseG "1"))

conTest =
 LC.testProperty "con test" (exampleCon == (parseG "(f * 1)"))

funTest =
 LC.testProperty "fun test" (exampleFun == (parseG "sin x"))

lawTest =
 LC.testProperty "law test" (exampleLaw == (Main.parseL "zeros: x + 0 = x"))

lawNameTest =
 LC.testProperty "calc test" ((calculate lawList exampleCalcExpr) == exampleCalc)



drv1 = Deriv ("x") (Con "+" [Var "x", Var "y"])
drv2 = Deriv ("y") (Con "+" [Var "x", Con "*" [Val 2, Var "y"]])




matchTest = 
 LC.testProperty "match test, should return nothing" (Expressions.match (drv1,drv2) == [])

sub1 = [(Var "x", (Con "+" [Var "x", Val 1]))]
sub2 = [(Var "x", (Con "+" [Var "x", Val 2]))]

unifyTest = 
 LC.testProperty "unify test, should return nothing" ((unify sub1 sub2) == [])





