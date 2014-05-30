-- | Digit factorials
-- Problem 34
-- http://projecteuler.net/problem=34

-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

-- Find the sum of all numbers which are equal to the sum of the factorial of
-- their digits.

-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import Data.Digits

main :: IO ()
main = do
  putStrLn "Problem 034 - Digit factorials"
  putStrLn $ "Sum: " ++ (show $ sum digitFactorials)

factorials :: [Int]
factorials = 1 : zipWith (*) [1..] factorials

factorial :: Int -> Int
factorial = (factorials !!)

digs :: Int -> [Int]
digs = digits 10

factorialSum :: Int -> Int
factorialSum n = sum $ map factorial (digs n)

check :: [Int]
check = [10..50000]

digitFactorials :: [Int]
digitFactorials = [x | x <- check, x == factorialSum x]