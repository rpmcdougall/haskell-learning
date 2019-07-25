module Caesar where

import Data.Char


--
-- Implementing Caesar Cipher Encoding
--
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

--
-- Implementing Caesar Cipher Decoding 
--

-- Frequency table of approximate frequencies of letters in alphabet derived from a large text
table :: [Float]
table = [8.1, 1.5, 2.8, 4.5, 12.7, 2.2, 2.0, 6.1, 7.0, 
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- % Integer to floating point number. To determine percentage of an integer to another
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- Builds frequency table given a body of text
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs

-- Implementation of chi square statistic
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- Rotates the elements of a list n places to the left to wrap around to the start of the list
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- Given a String decoded using Caesar Cipher, decodes the string
crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs

--
-- Helper functions from previous section
--
-- Returns number or lower case letters in a given string
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- Number of character occurence in a given string
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x ==x']

-- Lists the positions at when an element occurs in a list
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']


