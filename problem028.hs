stuff i n m
    | i == m = 0
    | otherwise = n + (stuff (i + 1) (n + d) m)
    where d = ((i `div` 4)+1) * 2


main = do
    putStrLn "Project Euler Problem 28"
    print $ stuff 0 1 (1 + (500 * 4))
