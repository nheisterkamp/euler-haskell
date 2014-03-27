-- | Coin sums
-- Problem 31
-- http://projecteuler.net/problem=31

-- In England the currency is made up of pound, £, and pence, p, and there are
-- eight coins in general circulation:

-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- It is possible to make £2 in the following way:

-- 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
-- How many different ways can £2 be made using any number of coins?

total :: Int
total = 200

main :: IO ()
main = do
  putStrLn "Problem 031 - Coin sums"

  print $ length
    [ x
    | c200 <- [0..1]
    , c100 <- [0..2]
    , c50  <- [0..4]
    , c20  <- [0..10]
    , c10  <- [0..20]
    , c5   <- [0..50]
    , c2   <- [0..100]
    , c1   <- [0..200]
    , let x = c200*200 + c100*100 + c50*50 + c20*20 + c10*10 + c5*5 + c2*2 + c1
    , x == total
    ]
