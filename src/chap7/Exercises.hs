module Exercises where


-- [f x | x <- xs, p x] Re-xpress user map and filter
-- exerciseMapFilter :: (a -> a) -> (a -> a) -> [a] -> [a]
-- exerciseMapFilter f p xs = map f (filter p xs)


-- Define higher order functions from prelude
-- all' :: (a-> Bool) -> [Bool] -> Bool
all' p = and . map p 

-- any' :: (a -> Bool) -> [Bool] -> Bool
any' p = or . map p


takeWhile :: (a -> Bool)  -> [a] -> [a]
takeWhile p xs = [ x | x <- filter p xs]

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs = [x | x <- filter (not . p) xs]
