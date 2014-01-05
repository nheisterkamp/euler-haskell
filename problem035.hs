import Tools

import Data.List
import Data.Char (digitToInt, intToDigit)
import Data.Digits

primes = primesListTo 1000000

circularPrimes = [ x | x <- primes, rotationsPrime x == True ]

isPrime n = elem n primes

numRotations d = nub $ map (unDigits 10) (rotations (digits 10 d))

rotationsPrime d = all (isPrime) (numRotations d)

main :: IO ()
main =
    putStrLn $ show $ length $ circularPrimes
