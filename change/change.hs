import Data.List

coins :: [Int]
-- coins = [1,2,5,10,20,50,100,200]
coins = [3,4,5]

change :: Int
-- change = 543
change = 9

least :: Int -> [Int] -> [(Int,Int)]
least change coins = minimum $ every change coins

every :: Int -> [Int] -> [[(Int,Int)]]
every change coins = nub $ map sort $ map (possible change) $ permutations coins

possible :: Int -> [Int] -> [(Int,Int)]
possible 0 _  = []
possible _ [] = []
possible y (x:xs)
  | a > 0     = (a,x):next
  | otherwise = next
  where a     = y `div` x
        next  = possible (y-(a)*x) xs

main :: IO()
main = mapM_ (putStrLn . show) (every change coins)
