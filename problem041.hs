import Data.Numbers.Primes
import Data.List
import Data.Digits

checkPrimes :: [Integer]
checkPrimes = map (unDigits 10) $ permutations [1..7]

primes' :: [Integer]
primes' = [ x | x <- checkPrimes, isPrime x == True ]

main :: IO ()
main = putStrLn $ show $ maximum $ primes'
