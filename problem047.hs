-- | Distinct primes factors
-- Problem 47
-- http://projecteuler.net/problem=47

-- The first two consecutive numbers to have two distinct prime factors are:
-- 14 = 2 × 7
-- 15 = 3 × 5

-- The first three consecutive numbers to have three distinct prime factors are:
-- 644 = 2² × 7 × 23
-- 645 = 3 × 5 × 43
-- 646 = 2 × 17 × 19.

-- Find the first four consecutive integers to have four distinct prime factors.
-- What is the first of these numbers?

import           Data.List           (nub)
import           Data.Numbers.Primes

main :: IO ()
main = do
    putStrLn "Problem 47 - Distinct primes factors"
    print $ consecutivePrimeFactors 4 2

distinctPrimeFactors n = length $ nub $ primeFactors n


consecutivePrimeFactors amount start
    | not b = consecutivePrimeFactors amount (start+1)
    | otherwise = start
    where l = [start..(start+amount-1)]
          r = map (distinctPrimeFactors) l
          b = length (filter (== amount) r) == amount




