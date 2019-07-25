module Exercises where


sumOfSquares :: Int
sumOfSquares = sum [x^2 | x <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (m', n') | m' <- [0..m] , n' <- [0..n]]
             
square' :: Int -> [(Int, Int)]
square' n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate :: Int -> a -> [a]
replicate n s = [ s | _ <- [0..n]]




