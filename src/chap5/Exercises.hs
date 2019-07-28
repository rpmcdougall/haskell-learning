module Exercises where


sumOfSquares :: Int
sumOfSquares = sum [x^2 | x <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (m', n') | m' <- [0..m] , n' <- [0..n]]
             
square' :: Int -> [(Int, Int)]
square' n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate :: Int -> a -> [a]
replicate n s = [ s | _ <- [0..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n],
                       y <- [1..n],
                       z <- [1..n],
                       x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (factors x) - x == x]



-- Helper Functions from previous sections --

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
