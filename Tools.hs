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
isPalindrome s = s == reverse s

numIsPalindrome :: Integer -> Bool
numIsPalindrome n = isPalindrome (digits 10 n)


divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n-1], mod n x == 0]

abundance :: Integer -> Integer
abundance n = sum (divisors n) - n

isPerfect :: Integer -> Bool
isPerfect n = abundance n == 0

isAbundant :: Integer -> Bool
isAbundant n = abundance n > 0

isDeficient :: Integer -> Bool
isDeficient n = abundance n < 0

-- abundantsList :: [Integer]
abundantsList n = [x | x <- [12..n], isAbundant x]

--abundantSums :: Integer -> [(Integer, Integer)]
abundantSums n = [(a,b)
    | a <- abundantsList d
    , b <- abundantsList a
    , a + b == n
    ]
    where d = n `div` 2

hasAbundantSum :: Integer -> Bool
hasAbundantSum n = not (null (abundantSums n))



