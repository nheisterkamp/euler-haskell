-- | Pandigital products
-- Problem 32
-- http://projecteuler.net/problem=32

-- We shall say that an n-digit number is pandigital if it makes use
-- of all the digits 1 to n exactly once; for example, the 5-digit
-- number, 15234, is 1 through 5 pandigital.

-- The product 7254 is unusual, as the identity, 39 × 186 = 7254,
-- containing multiplicand, multiplier, and product is 1 through 9
-- pandigital.

-- Find the sum of all products whose multiplicand/multiplier/product
-- identity can be written as a 1 through 9 pandigital.

-- HINT: Some products can be obtained in more than one way so be sure
-- to only include it once in your sum.

import           Data.Digits
import           Data.List

main :: IO ()
main = do
    putStrLn "Problem 32 - Pandigital products"

    let ps = filter checkProduct $ concatMap (cuts 3) (pandigitals 9)
        ps' = map onlyProducts ps
        ps'' = nub ps'

    print $ sum ps''
    print ps''

pandigitals n = [x 
    | let d = [1..n]
    , x <- map (unDigits 10) (permutations d)
    ]

cuts p n = [[a', b', c']
    | let d = digits 10 n
          l = length d
    , a <- [1..(l-p)]
    , b <- [a..(l-p)]
    , c <- [b..(l-p)]
    , a+b+c == l
    , let a' = unDigits 10 $ take a d
          b' = unDigits 10 $ take b (drop a d)
          c' = unDigits 10 $ drop (a+b) d
    , a' < b'
    ]

checkProduct [a, b, c] = a*b == c

onlyProducts [a, b, c] = c
