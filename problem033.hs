-- | Digit canceling fractions
-- Problem 33
-- http://projecteuler.net/problem=33

-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician
-- in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which
-- is correct, is obtained by cancelling the 9s.

-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
--
-- There are exactly four non-trivial examples of this type of fraction, less
-- than one in value, and containing two digits in the numerator and
-- denominator.

-- If the product of these four fractions is given in its lowest common terms,
-- find the value of the denominator.

import Data.Ratio
import Data.Digits
import Data.List

main :: IO ()
main = do
	putStrLn "Problem 033 - Digit canceling fractions"
	print $ denominator $ product fractions

fractions =
	[ f
	| n <- [11..99], n `rem` 10 /= 0
	, d <- [11..99], d `rem` 10 /= 0
	, d > n
	, let dn = digits 10 n
	, let dd = digits 10 d
	, let dnd1 = elemIndices (dn !! 0) dd
	, let dnd2 = elemIndices (dn !! 1) dd
	, let f = n % d
	, length dnd1 > 0 && ((dn !! 1) % dd !! (abs ((head dnd1) - 1)) == f)
	|| length dnd2 > 0 && ((dn !! 0) % dd !! (abs ((head dnd2) - 1)) == f)
	]

