{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Expressions where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.List (intersperse)
import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe



type Parser = Parsec Void String


-- Datatypes

data Expr
    = Var String
    | Val Int
    | Con String [Expr]
    | Deriv String Expr deriving (Eq,Show)


{-

instance Show Expr where
 show (Var s) = s
 show (Val i) = show i
 show (Con v [e]) = " (" ++ init (tail (show v)) ++ " " ++ show e ++ ")"
 show (Con v es) = " (" ++ show (head es) ++ (foldl (++) "" (map (((" " ++ v ++ " ") ++) . show) (tail es))) ++ ")"
 show (Deriv v e) = "d/d" ++ show v ++ " " ++ show e
-}

instance Show Step where
 show (Step l e) = "=   {" ++ l ++ "}\n" ++ show e ++ "\n"

-- instance Show Calculation where
 -- show (Calc e steps) = "\n" ++ show e ++ "\n" ++ (foldl (++) "" (map show steps))


instance Show Law where
  show (Law ln eq) = "Law " ++ ln  ++ " " ++ show eq 


data Law = Law LawName Equation deriving (Eq)
type LawName = String
type Equation = (Expr, Expr)


data Calculation = Calc Expr [Step] deriving (Show)
data Step = Step LawName Expr


type Subst = [(Expr,Expr)]

emptySub = []
unitSub (Var p) (Val l) = [[(Var p,Val l)]]
unitSub (Var p) _ | p == "p" || p=="q" = []
unitSub v e = [[(v,e)]]



-- Parsing


-- parse white space and comments
sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)


-- parse token
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- parse symbols
symbol :: String -> Parser String
symbol = L.symbol sc

-- parse any string between parenthesis
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- parse expr
term :: Parser Expr
term = deriv
       <|> parens expr
       <|> pInteger
       <|> (atom >>= more)
       where
        more v@(_:d:_)
          | isDigit d = return (Var v)
          | otherwise = do {ts <- many (expr <* space); return (Con v ts)}
        more v = return (Var v)

-- parse integer
pInteger :: Parser Expr
pInteger = Val <$> lexeme L.decimal

-- parse expr
expr :: Parser Expr
expr = makeExprParser term operatorTable <?> "expression"

-- parse variable
atom :: Parser String
atom = ((:) <$> letterChar <*> many alphaNumChar) <* space

-- parse derivative
deriv :: Parser Expr
deriv = do {string "d/d"; v <- atom; e <- expr; return (Deriv v e)}

-- parser that parses any string until char c
upto :: Char -> Parser String
upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c)

-- law parser
law :: Parser Law
law = do {name <- upto ':'; space; e1 <- expr; space; char '='; space; e2 <- expr; space; return (Law name (e1,e2))}


-- operators we support
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "sin" (func "sin")
    , prefix "cos" (func "cos")
    , prefix "ln" (func "ln") 
    , prefix "-" (func "-")]
  , [ binary "^" (funcc "^") ]
  , [ binary  "*" (funcc "*")          --[ InfixL (prod <$ symbol "*")
    , binary  "/"  (funcc "/")  ]
  , [ binary  "+"  (funcc "+")
    , binary  "-"  (funcc "-") ]
  ]


-- function for prefixes
func::String -> Expr -> Expr
func name a = Con name [a]

-- function for binary func

funcc::String -> Expr -> Expr -> Expr
funcc name a b = Con name [a,b]

binary  name f = InfixL  (f <$ symbol name)
prefix  name f = Prefix  (f <$ symbol name)


-- match left side of a law (e1) to an expr we are computing
match :: (Expr,Expr) -> [Subst]
match (Var v,e) = unitSub (Var v) e
match (Val v1,Val v2) = [[] | v1 == v2]
match (Con v1 e1,Con v2 e2)
  | v1==v2 = combine (map Expressions.match (zip e1 e2))
-- match (Deriv v1 e1,Deriv v2 e2) = map (derivWrap v1 v2) (Expressions.match (e1,e2))
  -- [ [(Deriv v1 s1, Deriv v2 s2)] | [(s1,s2)]<-(Expressions.match (e1,e2))]
match (Deriv v1 es1, Deriv v2 es2) = [varSubs ++ exprSubs | varSubs <- varMatch v1 v2, exprSubs <- Expressions.match (es1,es2), compatible varSubs exprSubs]
-- match (Deriv v1 es1,Deriv v2 es2)
--    = combine (map Expressions.match (zip es1 es2))
match _ = []

-- derivWrap :: Expr -> Expr -> Subst -> Subst
-- derivWrap v1 v2 [(e1,e2)] = [(Deriv v1 e1,Deriv v2 e2)]

varMatch :: String -> String -> [Subst]
varMatch v1 v2 = unitSub (Var v1) (Var v2)

-- check if two sets of subs are compatible, i.e. they are not when two respective subs refer to the same variable with different expr
compatible :: Subst -> Subst -> Bool
compatible subst1 subst2 = and [e1 == e2 | (v1,e1)<-subst1, (v2,e2)<-subst2, v1 == v2]

-- combine two sets of subs
union :: Subst -> Subst -> Subst
union [] sub2 = sub2
union sub1 [] = sub1
union (x:sub1) (y:sub2) = x:y:union sub1 sub2


-- combine two sets of subs if they are compatible
unify :: Subst -> Subst -> [Subst]
unify sub1 sub2 = [sub1 ++ sub2 | compatible sub1 sub2]

-- take a list of subs, and then apply unify to them and then merge that into a single list
unifyAll :: [Subst] -> [Subst]
unifyAll = foldr f [emptySub]
  where f sub subs = concatMap (unify sub) subs

combine :: [[Subst]] -> [Subst]
combine = concatMap unifyAll . cp

-- cartesian product of list of lists
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
                 where yss = cp xss


-- once we found a substition, apply the right side of the law to the expression we are computing
apply :: Subst -> Expr -> Expr
apply sub (Val v) = (Val v)
apply sub (Con v es) = Con v (map (apply sub) es)
apply sub (Deriv v e) = Deriv v (apply sub e)
-- apply sub (Deriv v es) = error "Cannot do subst in deriv yet (line 201)"
apply sub (Var e) = binding sub (Var e)

-- extractV :: Subst -> String
-- extractV sub = 

-- looks up a in [(a,b)], and then if found returns b, otherwise throws an error
binding :: Subst -> Expr -> Expr
binding sub e = case (lookup e sub) of
                  Nothing -> error ("Could not find a way of substituting "++show e)
                  Just v -> v


rewrites :: Equation -> Expr -> [Expr]
rewrites eqn (Var v) = []
rewrites eqn (Val v) = tlrewrite eqn (Val v)
rewrites eqn (Con v es)
       = tlrewrite eqn (Con v es)  ++  map (Con v) (anyOne (rewrites eqn) es)
rewrites eqn (Deriv v e)
       = tlrewrite eqn (Deriv v e)
-- rewrites eqn (Deriv v e) = tlrewrite eqn (Deriv v e)

-- top level rewrite, does most "meat" of the rewrites
tlrewrite :: Equation -> Expr -> [Expr]
tlrewrite (e1, e2) e = [apply sub e2 | sub <- subs]
                        where subs = Expressions.match (e1,e)



-- applies a function that object of type 'a' and returns a list of objects of type 'a' ([f1(a), ..., fn(a)]) and then 
-- applies that function to a list of objects of type 'a', [a],  x = [x1,x2,x3,...,xn]
-- as it does so, it returns a separate list for each elem within list x, e.g. [f1(x1), x2, x3, ..., xn] ++  [f2(x1), x2, x3, ..., xn] ++ ...
anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne f []     = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++
                  [x:xs' | xs' <- anyOne f xs]



-- return list of steps
steps :: Calculation -> [Step]
steps (Calc _ s) = s

-- apply laws to an expr, one by one in order they were written in a text file
calculate :: [Law] -> Expr -> Calculation
calculate laws e = Calc e (manyStep rws e)
  where rws e = [(Step name (eval e'))
                | Law name eqn <- laws,
                  e' <- rewrites eqn e,
                  e' /= e]

isVal :: Expr -> Bool
isVal (Val _) = True
isVal _ = False

pmm :: Expr -> Bool
pmm (Con v es) = v == "+"

vall:: Expr -> Int
vall (Val x) = x




--hasSameNum:: Expr -> [Int]
--hasSameNum (Con v [e1,e2]) = if v == "*"
--                             then if isVal e1 then [vall e1] 
--                                  else [vall e2]
--                             else []
--hasSameNum _ = []                   
 
--sortExprList:: Expr -> [Int]
--sortExprList (Var _) = []
--sortExprList (Val _) = []
--sortExprList (Con v [e1,e2]) = sortExprList e1 ++ (hasSameNum e2) 
--sortExprList (Deriv s e) =  sortExprList e


eval :: Expr -> Expr
eval (Deriv s e) = Deriv s (eval e)
eval (Con v [Val v1, Val v2])
  | v == "+" = (Val (v1 + v2))
  | v == "*" = (Val (v1 * v2))
  | v == "^" = (Val (v1 ^ v2))
eval (Con "*" [Val v1, Con v (x:xs)])
  |  isVal x = Con v (map eval ((Val ((vall x)*v1)):xs))
eval (Con v ls) = Con v (map eval ls)
eval e = e

-- return final step of the calculation?
manyStep :: (Expr -> [Step]) -> Expr -> [Step]
manyStep rws e
  = if null steps then []
    else step : manyStep rws (unpackStep step)
    where steps = rws e
          step = head steps

-- returns expr within a Step
unpackStep :: Step -> Expr
unpackStep (Step lawname e) = e


