module Exercises where


sumOfSquares :: Int
sumOfSquares = sum [x^2 | x <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (m', n') | m' <- [0..m] , n' <- [0..n]]
             
square' :: Int -> [(Int, Int)]
square' n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate' :: Int -> a -> [a]
replicate' n s = [ s | _ <- [0..n - 1]]

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

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n],
                       y <- [1..n],
                       z <- [1..n],
                       x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (factors x) - x == x]

-- ex7 = [(x,y) | x <- [1,2], y <- [3,4]]
-- ex7 :: [Int] -> [Int] -> [(Int, Int)]
ex7 xs ys = concat [[(x, y) | y <- ys] | x <- xs]

-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x', i) <- zip xs [0..], x == x']
positionsFind :: Eq a => a -> [a] -> [Int]
positionsFind x xs = find x (zip xs [0..])

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

-- Helper Functions from previous sections --

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
