-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- | Sub-string divisibility
-- Problem 43
-- http://projecteuler.net/problem=43

-- The number, 1406357289, is a 0 to 9 pandigital number because it is made up
-- of each of the digits 0 to 9 in some order, but it also has a rather
-- interesting sub-string divisibility property.

-- Let d_1 be the 1st digit, d_2 be the 2nd digit, and so on. In this way, we
-- note the following:

-- d_2 d_3 d_4  = 406 is divisible by 2
-- d_3 d_4 d_5  = 063 is divisible by 3
-- d_4 d_5 d_6  = 635 is divisible by 5
-- d_5 d_6 d_7  = 357 is divisible by 7
-- d_6 d_7 d_8  = 572 is divisible by 11
-- d_7 d_8 d_9  = 728 is divisible by 13
-- d_8 d_9 d_10 = 289 is divisible by 17

-- Find the sum of all 0 to 9 pandigital numbers with this property.

import Data.Digits
import Data.List
import Data.Numbers.Primes

main :: IO ()
main = do
  putStrLn "Problem 043 - Sub-string divisibility"

  print $ sum
    [ unDigits 10 d
    | d <- pandigitals
    , checkDigits d
    ]

  return ()

pandigitals :: [[Int]]
pandigitals = permutations [0..9]

splice :: [a] -> Int -> Int -> [a]
splice xs l s = take l . drop s $ xs

splices :: [a] -> Int -> Int -> [[a]]
splices xs l s = map (splice xs l) [s..(length xs - l)]

none :: [Int] -> Bool
none xs = maximum xs == 0

checkDigits :: [Int] -> Bool
checkDigits ds =
  none o
  where splices' = splices ds 3 1
        xs = map (unDigits 10) splices'
        o = zipWith mod xs primes
