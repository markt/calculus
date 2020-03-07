module Utilities where

-- applies a function that object of type 'a' and returns a list of objects of type 'a' ([f1(a), ..., fn(a)]) and then 
-- applies that function to a list of objects of type 'a', [a],  x = [x1,x2,x3,...,xn]
-- as it does so, it returns a separate list for each elem within list x, e.g. [f1(x1), x2, x3, ..., xn] ++  [f2(x1), x2, x3, ..., xn] ++ ...
anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne _ []     = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++
                  [x:xs' | xs' <- anyOne f xs]





-- cartesian product of list of lists
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
                 where yss = cp xss