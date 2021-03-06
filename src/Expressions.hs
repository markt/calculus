{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Expressions where

import Control.Monad.Combinators.Expr
import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor.Identity

import Parsing
import Utilities



-- Datatypes

data Expr
    = Var String
    | Val Int
    | Con String [Expr]
    | Deriv String Expr deriving (Eq,Show)

data Law = Law LawName Equation deriving (Eq)
type LawName = String
type Equation = (Expr, Expr)

data Calculation = Calc Expr [Step] deriving (Eq, Show)
data Step = Step LawName Expr deriving (Eq, Show)

type Subst = [(Expr,Expr)]

emptySub :: [a]
emptySub = []

unitSub :: Expr -> Expr -> [[(Expr, Expr)]]
unitSub (Var p) (Val l) = [[(Var p,Val l)]]
unitSub (Var p) _ | p == "p" || p=="q" = []
unitSub v e = [[(v,e)]]


-- Parsing

-- parse expr
term :: Parser Expr
term = deriv
       <|> parens expr
       <|> pInteger
       <|> (var >>= more)
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

-- parse derivative
deriv :: Parser Expr
deriv = do {_ <- string "d/d"; v <- var; e <- expr; return (Deriv v e)}

-- law parser
law :: Parser Law
law = do {name <- upto ':'; space; e1 <- expr; space; _ <- char '='; space; e2 <- expr; space; return (Law name (e1,e2))}


-- operators we support
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "sin" (prefixFunc "sin")
    , prefix "cos" (prefixFunc "cos")
    , prefix "ln" (prefixFunc "ln") 
    , prefix "-" (prefixFunc "-")]
  , [ binary "^" (binaryFunc "^") ]
  , [ binary  "*" (binaryFunc "*")
    , binary  "/"  (binaryFunc "/")  ]
  , [ binary  "+"  (binaryFunc "+")
    , binary  "-"  (binaryFunc "-") ]
  ]


-- function for prefixes
prefixFunc::String -> Expr -> Expr
prefixFunc name a = Con name [a]

-- function for binary func
binaryFunc::String -> Expr -> Expr -> Expr
binaryFunc name a b = Con name [a,b]

binary :: String
          -> (a -> a -> a) -> Operator (ParsecT Void String Identity) a
binary  name f = InfixL  (f <$ symbol name)

prefix :: String
          -> (a -> a) -> Operator (ParsecT Void String Identity) a

prefix  name f = Prefix  (f <$ symbol name)


-- match left side of a law (e1) to an expr we are computing
match :: (Expr,Expr) -> [Subst]
match (Var v,e) = unitSub (Var v) e
match (Val v1,Val v2) = [[] | v1 == v2]
match (Con v1 e1,Con v2 e2)
  | v1==v2 = combine (map Expressions.match (zip e1 e2))
match (Deriv v1 es1, Deriv v2 es2) = [varSubs ++ exprSubs | varSubs <- varMatch v1 v2, exprSubs <- Expressions.match (es1,es2), compatible varSubs exprSubs]
match _ = []


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


-- once we found a substition, apply the right side of the law to the expression we are computing
apply :: Subst -> Expr -> Expr
apply _ (Val v) = (Val v)
apply sub (Con v es) = Con v (map (apply sub) es)
apply sub (Deriv v e) = Deriv v (apply sub e)
-- apply sub (Deriv v es) = error "Cannot do subst in deriv yet (line 201)"
apply sub (Var e) = binding sub (Var e)


-- looks up a in [(a,b)], and then if found returns b, otherwise throws an error
binding :: Subst -> Expr -> Expr
binding sub e = case (lookup e sub) of
                  Nothing -> error ("Could not find a way of substituting "++show e)
                  Just v -> v


rewrites :: Equation -> Expr -> [Expr]
rewrites _ (Var _) = []
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



-- return list of steps
steps :: Calculation -> [Step]
steps (Calc _ s) = s

-- apply laws to an expr, one by one in order they were written in a text file
calculate :: [Law] -> Expr -> Calculation
calculate laws e = Calc e (manyStep rws e)
  where rws ee = [(Step name (eval e'))
                | Law name eqn <- laws,
                  e' <- rewrites eqn ee,
                  e' /= ee]

-- checks whether expr is Val
isVal :: Expr -> Bool
isVal (Val _) = True
isVal _ = False

-- returns a value contained within Val, since we have other possible expressions, I had to resort to returning a list
vall:: Expr -> [Int]
vall (Val x) = [x]
vall _ = []


-- shortens expressions by evaluation expressions that only consist of values
eval :: Expr -> Expr
eval (Deriv s e) = Deriv s (eval e)
eval (Con v [Val v1, Val v2])
  | v == "+" = (Val (v1 + v2))
  | v == "*" = (Val (v1 * v2))
  | v == "^" = (Val (v1 ^ v2))
  | v == "-" = (Val (v1 - v2))
eval (Con "*" [Val v1, Con v (x:xs)])
  |  isVal x = eval (Con v ((Val (head (vall x)*v1)):xs))
eval (Con v ls) = Con v (map eval ls)
eval e = e

-- return final step of the calculation
manyStep :: (Expr -> [Step]) -> Expr -> [Step]
manyStep rws e
  = if null stepss then []
    else stepp : manyStep rws (unpackStep stepp)
    where stepss = rws e
          stepp = head stepss

-- returns expr within a Step
unpackStep :: Step -> Expr
unpackStep (Step _ e) = e