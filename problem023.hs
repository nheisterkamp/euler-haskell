-- | Problem 23 - Non-abundant sums
-- http://projecteuler.net/problem=23

-- A perfect number is a number for which the sum of its proper divisors is 
-- exactly equal to the number. For example, the sum of the proper divisors 
-- of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect 
-- number.

-- A number n is called deficient if the sum of its proper divisors is less 
-- than n and it is called abundant if this sum exceeds n.

-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest 
-- number that can be written as the sum of two abundant numbers is 24. By 
-- mathematical analysis, it can be shown that all integers greater than 28123 
-- can be written as the sum of two abundant numbers. However, this upper limit 
-- cannot be reduced any further by analysis even though it is known that the 
-- greatest number that cannot be expressed as the sum of two abundant numbers 
-- is less than this limit.

-- Find the sum of all the positive integers which cannot be written as the sum 
-- of two abundant numbers.

--import Tools
import Data.List
import Control.Monad

roof = 28124
-- roof = 1000

divisors n = [x | x <- [1..(n-1)], n `mod` x == 0]
abundant n = sum (divisors n) > n
abundants a b = [x | x <- [a..b], abundant x]

abundantSums a b = [z
    | x <- abundants 12 b
    , y <- abundants a (b-x)
    , let z = x + y
    , z >= a
    , z <= b
    ]

nonAbundantSums a b = [a..b] \\ abundantSums a b

step :: Integer
step = 100

-- stuff :: [Integer] -> IO Integer
stuff x = do
    let s = sum $ nonAbundantSums x (x+step)
    putStrLn $ show x ++ " : " ++ show s
    return s

-- main :: IO Integer
main = do
    putStrLn "Problem 23..."
    let m = [0,step..roof]
    total <- mapM stuff m
    putStrLn $ "Total: " ++ show (sum total)
