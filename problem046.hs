-- | Goldbach's other conjecture
-- Problem 46
-- http://projecteuler.net/problem=46

-- What is the smallest odd composite that cannot be written as the sum
-- of a prime and twice a square?


import           Data.Numbers.Primes

composites = [x | x <- [2..], not $ isPrime x]
oddComposites = [x | x <- composites, x `mod` 2 == 1]


goldbach n = [(x,y,z)
    | x <- takeWhile (< n) primes
    , y <- [1..d]
    , let z = x + (2 * (y^2))
    , z == n
    ]
    where d = n `div` 2 + 1

goldbachs = [x | x <- oddComposites, not $ null $ goldbach x]
notGoldbachs = [x | x <- oddComposites, null $ goldbach x]

main = do
    putStrLn "Project Euler: Problem 46 - Goldbach's other conjecture"
    print $ head notGoldbachs
