module Tools where

import Data.Numbers.Primes
import Data.Digits
import Data.List

primesListTo :: Integer -> [Integer]
primesListTo m = takeWhile (< m) primes

rotations :: [a] -> [[a]]
rotations xs = init (zipWith (++) (tails xs) (inits xs))

numRotations :: Integer -> [Integer]
numRotations d = nub $ map (unDigits 10) (rotations (digits 10 d))

numReverse :: Integer -> Integer
numReverse n = unDigits 10 $ reverse (digits 10 n)

isPalindrome :: [Integer] -> Bool
isPalindrome s = s == (reverse s)

numIsPalindrome :: Integer -> Bool
numIsPalindrome n = isPalindrome (digits 10 n)
