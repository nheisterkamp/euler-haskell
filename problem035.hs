import Tools

import Data.Numbers.Primes
import Data.List

primes' :: [Integer]
primes' = primesListTo 1000000

circularPrimes :: [Integer]
circularPrimes = [ x | x <- primes', rotationsPrime x == True ]

rotationsPrime :: Integer -> Bool
rotationsPrime d = all (isPrime) (numRotations d)

main :: IO ()
main = putStrLn $ show $ length $ circularPrimes
