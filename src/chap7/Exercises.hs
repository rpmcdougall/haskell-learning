module Exercises where


-- [f x | x <- xs, p x] Re-xpress user map and filter
-- exerciseMapFilter :: (a -> a) -> (a -> a) -> [a] -> [a]
-- exerciseMapFilter f p xs = map f (filter p xs)


-- Define higher order functions from prelude
-- all' :: (a-> Bool) -> [Bool] -> Bool
all' p = and . map p 

-- any' :: (a -> Bool) -> [Bool] -> Bool
any' p = or . map p


takeWhile' :: (a -> Bool)  -> [a] -> [a]
takewhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []
                   
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropwhile' _ [] = []
dropWhile' p (x:xs) | p x =  x : dropWhile' p xs
                    | otherwise = x : xs


-- redefine map f and filter p using foldr
map' f  = foldr (\x xs -> f x:xs) []
filter p = foldr(\x xs -> if p x then x:xs else xs) []




