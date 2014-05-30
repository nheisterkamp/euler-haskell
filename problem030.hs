-- | Digit fifth powers
-- Problem 30
-- http://projecteuler.net/problem=30

-- Surprisingly there are only three numbers that can be written as the sum of
-- fourth powers of their digits:

--   1634 = 14 + 64 + 34 + 44
--   8208 = 84 + 24 + 04 + 84
--   9474 = 94 + 44 + 74 + 44

-- As 1 = 14 is not a sum it is not included.

-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.

-- Find the sum of all the numbers that can be written as the sum of fifth
-- powers of their digits.

import Data.Digits

power :: Int
power = 5

offset :: Int
offset = 10

limit :: Int
limit = 1000000

check :: [Int]
check = [offset..limit]

main :: IO ()
main = do
	putStrLn "Problem 030 - Digit fifth powers"

	print $ checked
	putStrLn $ "Sum: " ++ (show $ sum checked)

	return ()

checked :: [Int]
checked =
	[ c
	| c <- check
	, c == powerSum power c
	]

powerSum :: Int -> Int -> Int
powerSum p n = sum $ map (^ p) (digits 10 n)