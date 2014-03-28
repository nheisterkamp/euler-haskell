-- | Champernowne's constant
-- Problem 40
-- http://projecteuler.net/problem=40

-- An irrational decimal fraction is created by concatenating the positive
-- integers:

-- 0.123456789101112131415161718192021...

-- It can be seen that the 12th digit of the fractional part is 1.

-- If dn represents the nth digit of the fractional part, find the value of the
-- following expression.

-- d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

import Data.Char

main :: IO ()
main = do
  putStrLn "Problem 040 - Champernowne's constant"

  print $ product $ map (digitToInt . (!!) result) digits

  where result = "0" ++ concatMap show [1..]
        digits = [1, 10, 100, 1000, 10000, 100000, 1000000]