module Exercises (halve) where

halve :: [a] -> ([a], [a])
halve xs =
  let sep = length xs `div` 2
      left = take sep xs
      right = take sep (reverse xs)
      in (left, reverse right)
