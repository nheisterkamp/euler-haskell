-- | Lattice Quadrilaterals
-- Problem 453
-- http://projecteuler.net/problem=453
--
-- A simple quadrilateral is a polygon that has four distinct vertices, has
-- no straight angles and does not self-intersect.
--
-- Let Q(m, n) be the number of simple quadrilaterals whose vertices are
-- lattice points with coordinates (x,y) satisfying 0 ≤ x ≤ m and 0 ≤ y ≤ n.
--
-- For example, Q(2, 2) = 94 as can be seen below:
-- It can also be verified that
-- * Q(3, 7) = 39590,
-- * Q(12, 3) = 309000 and
-- * Q(123, 45) = 70542215894646.
--
-- Find Q(12345, 6789) mod 135707531.
--

import           Data.List
import           Data.Matrix
import           Tools       (rotations)


main = do
    putStrLn "Problem 453.."
    let q = quadris 2 2

    mapM_ (printQuadri 2 2) (q)
    putStrLn $ "Length: " ++ show (length q)
    putStrLn $ "Length unique: " ++ show (length $ nub q)


quadris m n = concat
    [ r
    | x1 <- [0..(m-1)], y1 <- [0..(n-1)]
    , x2 <- [1..m], y2 <- [0..n]
    , x3 <- [0..m], y3 <- [1..n]
    , x4 <- [1..m], y4 <- [1..n]
    , let p1 = (x1, y1)
          p2 = (x2, y2)
          p3 = (x3, y3)
          p4 = (x4, y4)
    -- Don't allow identical points
    , p1 /= p2 && p1 /= p3 && p1 /= p4
    ,             p2 /= p3 && p2 /= p4
    ,                         p3 /= p4
    -- Always move forward
    -- p2 top right
    , x2 >= x1 && y2 >= y1

    -- p3 left bottom
    -- , y3 >= y1 && y3 >= y2
    -- , x3 /= y2 && y3 /= x2
 
    -- p4 right bottom
    , x4 >= x3 && y4 >= y2

    , let q = [p1, p2, p3, p4]
    --      r = [q] 
          r = simplePaths q
    ]

angle u v = acos $ dot (unit u) (unit v)
angleP a b c = angle (vector a b) (vector b c)
angleL [a, b, c] = angleP a b c

dot u v = (fst u * fst v) + (snd u * snd v)
dotP a b c = dot (vector a b) (vector b c)

lengthV :: (Int, Int) -> Double
lengthV v = sqrt $ (fromIntegral $ fst v)^2 + (fromIntegral $ snd v)^2
lengthP a b c = lengthV (vector a b)

vector a b = ((fst b - fst a), (snd b - snd a))
unit v = (fromIntegral (fst v) / l, fromIntegral (snd v) / l)
    where l = lengthV v

approx a b = (a-d < b) && (a+d > b)
    where d = 10^^(-6)

codirectional u v = False

angles q = map angleL $ map (take 3) (rotations q)

paths q = map (head q :) (rotations $ tail q)

simple q = sum (as) `approx` (2*pi) && (null $ filter (approx 0) as)
    where as = angles q

simplePaths q = filter (simple) (paths q)



printQuadri m n q = do
    putStrLn $ "Quadri: " ++ (show q)
    print $ angles q
    putStr $ prettyMatrix $ matrix (m+1) (n+1) $
        \(i,j) -> if null $ filter ((i-1,j-1) ==) q then 0 else 1


