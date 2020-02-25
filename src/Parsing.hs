{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}
module Parsing where

import Data.Char
-- import Data.String
import Data.Void
import Data.Text (Text)
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char

newtype Parser a = Parser (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (Parser p) s = p s

parse :: Parser a -> String -> a
parse p = fst . head . apply p

-- instance Monad Parser where
--     return x = Parser (\s -> [x,s])
--     p >>= q  = Parser (\s -> [(y,s'')
--                              | (x,s') <- apply p s,
--                                (y,s'') <- apply (q x) s'])


instance Monad Parser where
    return x = Parser (\s -> [(x,s)])
    Parser p >>= f
        = Parser (\s -> [ (y,s'')
                        | (x,s')<-p s
                        , (y,s'')<-apply (f x) s'])

instance Applicative Parser where
    pure = return
    Parser p <*> Parser q
        = Parser (\s -> [ (x y,s'')
                        | (x,s')<-p s
                        , (y,s'')<-q s'])

instance Functor Parser where
    fmap f = (pure f <*>)
 

getc :: Parser Char
getc = Parser f
    where f [] = []
          f (x:xs) = [(x,xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- getc;
            if p c then return c
            else Parsing.fail}

fail :: Parser a
fail = Parser (\s -> [])


failP :: Parser a
failP = Parser (const [])

-- guard :: Bool -> Parser ()
-- guard True = return ()
-- guard False = Parsing.fail


-- char :: Char -> Parser ()
-- char x = do {c <- sat (==x); return ()}

-- string :: String -> Parser ()
-- string []     = return ()
-- string (x:xs) = do {char x; string xs; return ()}

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = do {d <- sat isDigit; return (cvt d)}
        where cvt d = fromEnum d - fromEnum '0'

-- (<|>) :: Parser a -> Parser a -> Parser a
-- p <|> q = Parser f
--           where f s = let ps = apply p s in
--                       if null ps then apply q s
--                       else ps

-- lowers :: Parser String
-- lowers = do {c <- lower; cs <- lowers; return (c:cs)}
--          <|> return ""

-- lowers = many lower

-- wrong :: Parser Int
-- wrong = digit <|> addition

-- better :: Parser Int
-- better = addition <|> digit

-- best = digit >>= rest
-- rest m = do {char '+'; n <- digit; return (m+n)}
--          <|> return m

-- addition :: Parser Int
-- addition = do {m <- digit; char '+'; n <- digit;
--                return (m+n)}

-- many :: Parser a -> Parser [a]
-- many p = do {x <- p; xs <- many p; return (x:xs)}
--          <|> none
-- none = return []

-- space :: Parser ()
-- space = many (sat isSpace) >> return ()

-- symbol :: String -> Parser ()
-- symbol xs = space >> string xs

-- token :: Parser a -> Parser a
-- token p = space >> p

-- some :: Parser a -> Parser [a]
-- some p = do {x <- p; xs <- many p; return (x:xs)}

-- many :: Parser a -> Parser [a]
-- many p = optional (some p)
-- none = return []

-- optional :: Parser [a] -> Parser [a]
-- optional p = p <|> none

-- natural :: Parser Int
-- natural = token nat
-- nat = do {ds <- Parsing.some digit;
--           return (foldl1 shiftl ds)}
--       where shiftl m n = 10*m+n

-- -- int :: Parser Int
-- -- int = do {symbol "-"; n <- natural; return (-n)}
-- --       <|> natural

-- int :: Parser Int
-- int = do {space; f <- minus; n <- nat; return (f n)}
--     where
--     minus = (char '-' >> return negate) <|> return id

-- ints :: Parser [Int]
-- ints = bracket (manywith (symbol ",") int)

-- bracket :: Parser a -> Parser a
-- bracket p = do {symbol "[";
--                 x <- p;
--                 symbol "]";
                -- return x}

-- manywith :: Parser b -> Parser a -> Parser [a]
-- manywith q p = optional (somewith q p)

-- somewith :: Parser b -> Parser a -> Parser [a]
-- somewith q p = do {x <- p;
--                    xs <- many (q >> p);
--                    return (x:xs)}

-- makeExprParser :: Parser Expression -> 



-- operator :: Parser String
-- operator = try  