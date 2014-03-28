-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- | Integer right triangles
-- Problem 39
-- http://projecteuler.net/problem=39

-- If p is the perimeter of a right angle triangle with integral length sides,
-- {a,b,c}, there are exactly three solutions for p = 120.

-- {20,48,52}, {24,45,51}, {30,40,50}

-- For which value of p ≤ 1000, is the number of solutions maximised?

import Data.List
import Data.Ord

main :: IO ()
main = do
  putStrLn "Problem 039 - Integer right triangles"

  let triangles =
        [ t
        | p <- [0..1000]
        , let t = trianglesP p
        ]

  print $ maxi triangles

  return ()

maxi xs = maximumBy (comparing fst) (zip xs [0..])

isTriangleP a b p = (a + b + (sqrt (a^2 + b^2))) == p

trianglesP p = length
    [ 1
    | a <- [1..(p/2)]
    , b <- [a..(p/2)]
    , let c = p - b - a
    , c > b
    , isTriangleP a b p
    ]
