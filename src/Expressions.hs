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

-- newtype Expr = Compose [Atom] deriving Eq
data Expr
    = Var String
    | Val Int
    | Con String [Expr] deriving (Eq)
-- TODO: add deriv type: Deriv String [Expr]

-- data Law = Law String Equation deriving (Show, Eq)
-- type Equation = (Expr, Expr)


data Law = Law LawName Equation deriving (Eq)
type LawName = String
type Equation = (Expr, Expr)


data Calculation = Calc Expr [Step]
data Step = Step LawName Expr
-- data Step = Step String Expr



instance Show Expr where
 show (Var s) = "Var " ++  s
 show (Val i) = show i
 show (Con v es) = v ++ " (" ++ (foldl (++) "" (map ((" " ++) . show) es)) ++ ")"


instance Show Step where
 show (Step l e) = "= {" ++ l ++ "}\n" ++ show e ++ "\n"

instance Show Calculation where
 show (Calc e steps) = "\n " ++ show e ++ "\n" ++ (foldl (++) "" (map show steps))


instance Show Law where
  show (Law ln eq) = "Law " ++ ln  ++ " " ++ show eq 


type Subst = [(Expr,Expr)]
-- type VarName = String

emptySub = []
unitSub v e = [(v,e)]

-- unitSub :: Expr -> Expr -> Subst
-- unitSub v e = [(v,e)]





-- Parsing

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
-- parens x = string "(" *> x <* string ")" <* space


term :: Parser Expr
term = parens expr
       <|> pInteger
       <|> (atom >>= more)
       where
        more v@(_:d:_)
          | isDigit d = return (Var v)
          | otherwise = do {ts <- many (expr <* space); return (Con v ts)}
        more v = return (Var v)

pInteger :: Parser Expr
pInteger = Val <$> lexeme L.decimal

expr :: Parser Expr
expr = makeExprParser term operatorTable <?> "expression"

atom :: Parser String
atom = ((:) <$> letterChar <*> many alphaNumChar) <* space



-- TODO add function application support e.g. sin(x)
-- consider only apply for unary, makeexprparser for binary
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "sin" (func "sin")]
  , [ binary "^" (funcc "^") ]
  , [ binary  "*" (funcc "*")          --[ InfixL (prod <$ symbol "*")
    , binary  "/"  (funcc "/")  ]
  , [ binary  "+"  (funcc "+")
    , binary  "-"  (funcc "-") ]
  ]


func::String -> Expr -> Expr
func name a = Con name [a]


funcc::String -> Expr -> Expr -> Expr
funcc name a b = Con name [a,b]



binary  name f = InfixL  (f <$ symbol name)
prefix  name f = Prefix  (f <$ symbol name)

law :: Parser Law
law = do {name <- upto ':'; space; e1 <- expr; space; char '='; space; e2 <- expr; space; return (Law name (e1,e2))}


upto :: Char -> Parser String
upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c)

splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (a:as) = [([],a:as)] ++ [(a:as1,as2) | (as1,as2) <- splitsH as]
splitsH as = [(take n as,drop n as) | n <- [0..length as]]

splitsN :: Int -> [a] -> [[[a]]]
splitsN 0 [] = [[]]
splitsN 0 as = []
splitsN n as = [bs : bss | (bs,cs) <- splits as, bss <- splitsN (n-1) cs]


splitsAll :: [a] -> [[[a]]]
splitsAll as = splitsN (length as) as




-- allignments :: (Expr,Expr) -> [[(Expr,Expr)]]
-- allignments ((Con v1 e1),(Con v2 e2)) = [zip e1 (map (Con v1) e2s) | e2s <- splitsN n e2]
--                                       where n = length e1


-- match :: (Expr,Expr) -> [Subst]
-- match = concatMap (map matchE) . allignments
-- match (Var v,e) = [unitSub (Var v) e]

match :: (Expr,Expr) -> [Subst]
match (Var v,e) = [unitSub (Var v) e]
match (Val v1,Val v2) = [[] | v1 == v2]
match (Con v1 e1,Con v2 e2)
  | v1==v2 = combine (map Expressions.match (zip e1 e2))
match _ = []

unify :: Subst -> Subst -> [Subst]
unify sub1 sub2 = [sub1 ++ sub2 | compatible sub1 sub2]
-- unify sub1 sub2 = if compatible sub1 sub2
--                   then [union sub1 sub2]
--                   else []

compatible :: Subst -> Subst -> Bool
compatible subst1 subst2 = and [e1 == e2 | (v1,e1)<-subst1, (v2,e2)<-subst2, v1 == v2]

union :: Subst -> Subst -> Subst
union [] sub2 = sub2
union sub1 [] = sub1
union (x:sub1) (y:sub2) = x:y:union sub1 sub2


unifyAll :: [Subst] -> [Subst]
unifyAll = foldr f [emptySub]
  where f sub subs = concatMap (unify sub) subs

combine :: [[Subst]] -> [Subst]
combine = concatMap unifyAll . cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
                 where yss = cp xss


apply :: Subst -> Expr -> Expr
apply sub e = binding sub e


binding :: Subst -> Expr -> Expr
binding sub e = fromJust (lookup e sub)


rewrites :: Equation -> Expr -> [Expr]
rewrites eqn (Var v) = []
rewrites eqn (Val v) = tlrewrite eqn (Val v)
rewrites eqn (Con v es)
       = tlrewrite eqn (Con v es)  ++  map (Con v) (anyOne (rewrites eqn) es)

tlrewrite :: Equation -> Expr -> [Expr]
tlrewrite (e1, e2) e = [apply sub e2 | sub <- subs]
                        where subs = Expressions.match (e1,e)




anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne f []     = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++
                  [x:xs' | xs' <- anyOne f xs]




steps :: Calculation -> [Step]
steps (Calc _ s) = s

calculate :: [Law] -> Expr -> Calculation
calculate laws e = Calc e (manyStep rws e)
  where rws e = [(Step name e')
                | Law name eqn <- laws,
                  e' <- rewrites eqn e,
                  e' /= e]

manyStep :: (Expr -> [Step]) -> Expr -> [Step]
manyStep rws e
  = if null steps then []
    else step : manyStep rws (unpackStep step)
    where steps = rws e
          step = head steps

unpackStep :: Step -> Expr
unpackStep (Step lawname e) = e


-- apply sub (Var v) = binding sub (Var v)
-- apply sub (Con v es) = binding sub (Con v es)
-- apply sub (Con v es) = Con v (map (apply sub) es)
-- applyE sub (Var v) = apply sub (Var v)
-- applyE sub (Con v es) = Con v (map (apply sub) es)

-- apply ([(Var "x",(Con "+" [Var "x", Val 0]))]) (Var "x")
-- apply ([((Con "+" [Var "x", Val 0]),Var "x")]) (Con "+" [Var "x", Val 0])
-- apply ([((Con "^" [Var "x", Var "y"]),(Con "*" [Var "y", Var "x"]))]) (Var "x")
-- apply ([(Var "sin",(Con ""))])

-- match (Con v1 es1, Con v2 es2) | v1 == v2
--   = combine (map match (zip es1 es2))

