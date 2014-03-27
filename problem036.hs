-- | Double-base palindromes
-- Problem 36
-- http://projecteuler.net/problem=36

-- The decimal number, 585 = 1001001001_2 (binary), is palindromic in both
-- bases.

-- Find the sum of all numbers, less than one million, which are palindromic in
-- base 10 and base 2.

-- (Please note that the palindromic number, in either base, may not include
-- leading zeros.)

import Numeric
import Data.Char

main :: IO ()
main = do
  putStrLn "Problem 036 - Double-base palindromes"

  print $ sum
    [ c
    | c <- numbers
    , let b = toBinary c
    , let d = toDecimal c
    , isPalindrome b == True
    , isPalindrome d == True
    ]

numbers :: [Int]
numbers = [1..1000000]

toDecimal :: Int -> [Char]
toDecimal n = showIntAtBase 10 intToDigit n ""

toBinary :: Int -> [Char]
toBinary n = showIntAtBase 2 intToDigit n ""

isPalindrome :: [Char] -> Bool
isPalindrome s =
  a == b
  where
    d = (length s) `quot` 2
    a = take d s
    b = take d $ reverse s
