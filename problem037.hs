import Tools
import Data.Numbers.Primes
import Data.List
import Data.Digits

primes' :: [Integer]
primes' = primes''
	where primes'' = [ x | x <- primes, x > 9 ]

leftPrimes  = [ x | x <- primes', (allPrimes . numTails) x == True ]
rightPrimes = [ x | x <- primes', (allPrimes . numInits) x == True ]
bothPrimes  = [ x | x <- leftPrimes, (allPrimes . numInits) x == True ]

main = putStrLn $ show $ sum $ take 11 bothPrimes
