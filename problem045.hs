-- | Triangular, pentagonal, and hexagonal
-- Problem 45

-- Triangle, pentagonal, and hexagonal numbers are generated 
-- by the following formulae:

-- Triangle     Tn=n(n+1)/2     1, 3, 6, 10, 15, ...
-- Pentagonal   Pn=n(3n−1)/2    1, 5, 12, 22, 35, ...
-- Hexagonal    Hn=n(2n−1)      1, 6, 15, 28, 45, ...
-- It can be verified that T285 = P165 = H143 = 40755.

-- Find the next triangle number that is also pentagonal and 
-- hexagonal.

main :: IO ()
main = do
  putStrLn "Problem 45 - Triangular, pentagonal, and hexagonal"
  print $ take 2 ls

t n = (n * (    n + 1)) `div` 2
p n = (n * (3 * n - 1)) `div` 2
h n =  n * (2 * n - 1)

findFNV f n v = not $ null $ filter (== v) $ takeWhile (<= v) $ map f [n..]

ls = [(h')
  | hn <- [2..]
  , let h' = h hn
  , findFNV p hn h'
  , findFNV h hn h'
  ]









