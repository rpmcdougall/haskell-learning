module Exercises where

{-|
   Recursion
   1. Define the type
   2. Enumerate the cases
   3. Define the simple cases
   4. Define the other cases
   5. Generalize and simplify
-}


facPositive :: Int -> Int
facPositive n
  | n == 0 = 1
  | n > 0 = n * facPositive (n - 1)


sumDown :: Int -> Int
sumDown n
  | n == 0 = 0
  | n == 1 = 1
  | n > 1 = n + sumDown(n - 1) 

(<^>) :: Int -> Int -> Int
0 <^> _ = 0
m <^> 0 = 1
m <^> 1 = m
m <^> n = m * ( m <^> (n - 1))


euclid :: Int -> Int -> Int
euclid a b
  | a == b = a
  | a < b = euclid a ( b - a )
  | b < a = euclid ( a - b ) b
