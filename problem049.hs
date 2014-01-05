-- | http://projecteuler.net/problem=49

-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
-- increases by 3330, is unusual in two ways:
-- (i) each of the three terms are prime, and,
-- (ii) each of the 4-digit numbers are permutations of one another.

-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
-- exhibiting this property, but there is one other 4-digit increasing sequence.

-- What 12-digit number do you form by concatenating the three terms in this
-- sequence?

import Tools
import Data.Numbers.Primes

--primes' :: [Integer]
primes' = [ x | x <- takeWhile (<= 9999) primes, x > 999 ]

--primesDiff n = map ((-) n) primes'
primesDiff =[ (a,b,c)
        | a <- primes',
          b <- numPermutations a,
          c <- numPermutations a,
          a /= b,
          b /= c,
          isPrime b,
          isPrime c,
          (c-b) == (b-a),
          (c-b) > 0
    ]

main = putStrLn $ show $ primesDiff
