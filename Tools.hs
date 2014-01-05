{-# OPTIONS_GHC -O2 #-}

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
