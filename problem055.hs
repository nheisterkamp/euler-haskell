import Tools

lychrel :: Integer -> Bool
lychrel n = lychrel' n 0
    where lychrel' n i 
            | (i > 0 && numIsPalindrome n) = False
            | i == 50 = True
            | otherwise = lychrel' (n + (numReverse n)) (i + 1)


checkList :: [Integer]
checkList = [1..9999]

lychrelList :: [Bool]
lychrelList = map (lychrel) checkList


main :: IO ()
main = putStrLn $ show $ length $ filter (== True) lychrelList
