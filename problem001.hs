main :: IO ()
main = do
  putStrLn "Problem 001"

  putStrLn $ (show . sum) x

  where x = [ x | x <- [1..1000]
            , x `mod` 3 == 0
            , x `mod` 5 == 0
            ]
