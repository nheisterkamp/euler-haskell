{-# LANGUAGE OverloadedStrings #-}
-- | Coded triangle numbers
-- Problem 42
-- http://projecteuler.net/problem=42

-- The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
-- so the first ten triangle numbers are:

-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

-- By converting each letter in a word to a number corresponding to its
-- alphabetical position and adding these values we form a word value. For
-- example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value
-- is a triangle number then we shall call the word a triangle word.

-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
-- containing nearly two-thousand common English words, how many are triangle
-- words?

import Data.Char (ord)
import Data.Set (member, fromList)
import Data.Text (splitOn, pack, unpack)

wordsFile :: String
wordsFile = "problem042.words.txt"

splitWords :: String -> [String]
splitWords s = map
  (tail . init . unpack) -- removes "s
  (splitOn "," $ pack s) -- splits on ,

wordNumber :: String -> Int
wordNumber s = sum $ map
  ((+) (-64) -- uppercase alphabet starts at 65
    . ord) s

triangle :: Int -> Int
triangle n = (n * (n + 1)) `div` 2

triangles :: [Int]
triangles = [ triangle n | n <- [1..100000] ]

isTriangle :: Int -> Bool
isTriangle = (`member` fromList triangles)

isTriangleWord :: String -> Bool
isTriangleWord = isTriangle . wordNumber

main :: IO ()
main = do
  putStrLn "Problem 042 - Coded triangle numbers"

  contents <- readFile wordsFile
  let words = splitWords $ contents

  putStr "Triangle words: "
  putStrLn $ show . length $
    [ w
    | w <- words
    , isTriangleWord w
    ]
