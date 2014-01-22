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

import Data.Matrix

data Point = Point
    { pointX :: Int
    , pointY :: Int
    } deriving (Show, Eq)

data Vertex = Vertex
    { vertexFrom :: Point
    , vertexTo   :: Point
    } deriving (Show, Eq)

data Vector = Vector
    { vectorX :: Int
    , vectorY :: Int
    } deriving (Show, Eq)

data UnitVector = UnitVector
    { vectorUX :: Double
    , vectorUY :: Double
    } deriving (Show, Eq)

vector :: Vertex -> Vector
vector v = Vector ((pointX $ vertexTo v) - (pointX $ vertexFrom v)) ((pointY $ vertexTo v) - (pointY $ vertexFrom v))

unitVector :: Vector -> UnitVector
unitVector v = UnitVector ((fromIntegral $ vectorX v) / vectorLength v) ((fromIntegral $ vectorY v) / vectorLength v)

dotVertex :: Vertex -> Vertex -> Int
dotVertex vx1 vx2 = dot (vector vx1) (vector vx2)

dot :: Vector -> Vector -> Int
dot v1 v2 = (vectorX v1 * vectorX v2) + (vectorY v1 * vectorY v2)

unitDot :: UnitVector -> UnitVector -> Double
unitDot v1 v2 = (vectorUX v1 * vectorUX v2) + (vectorUY v1 * vectorUY v2)


vectorLength :: Vector -> Double
vectorLength v = sqrt $ (fromIntegral $ vectorX v)^2 + (fromIntegral $ vectorY v)^2


approxEqDiff = 10^^(-5)

approxEq :: Double -> Double -> Bool
approxEq d1 d2 = ((d1 - approxEqDiff) < d2) && ((d1 + approxEqDiff) > d2)

codirectional v1 v2 = approxEq (fromIntegral $ dot v1 v2) (vectorLength v1 * vectorLength v2)

angle v1 v2 = acos $ unitDot (unitVector v1) (unitVector v2)

data Quadrilateral = Quadrilateral
    { p1 :: Point
    , p2 :: Point
    , p3 :: Point
    , p4 :: Point
    } deriving (Eq)

instance Show Quadrilateral where
    show q = (show $ (pointX (p1 q), pointY (p1 q))) ++ " -> " ++
                (show $ (pointX (p2 q), pointY (p2 q))) ++ " -> " ++
                (show $ (pointX (p3 q), pointY (p3 q))) ++ " -> " ++
                (show $ (pointX (p4 q), pointY (p4 q)))

simple q = approxEq (2*pi) a && null (filter (approxEq 0) as)
    where as = quadriAngles q
          a = sum as

complex = not . simple

quadriAngle q = sum $ quadriAngles q

quadriAngles q = [angle v1 v2, angle v2 v3, angle v3 v4, angle v4 v1]
    where v1 = vector $ Vertex (p1 q) (p2 q)
          v2 = vector $ Vertex (p2 q) (p3 q)
          v3 = vector $ Vertex (p3 q) (p4 q)
          v4 = vector $ Vertex (p4 q) (p1 q)


quadris m n = [Quadrilateral (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4)
    | x1 <- [0..(m-1)]
    , y1 <- [0..(n-1)]
    , x2 <- [(x1)..m]
    , y2 <- [(y1)..n]
    , x3 <- [(x1)..m]
    , y3 <- [(x1)..n]
    , x4 <- [(x1)..m]
    , y4 <- [(y1)..n]
    , (x1, y1) /= (x2, y2)
    , (x1, y1) /= (x3, y3)
    , (x1, y1) /= (x4, y4)
    , (x2, y2) /= (x3, y3)
    , (x2, y2) /= (x4, y4)
    , (x3, y3) /= (x4, y4)
    , y4 <= y2
    , x4 <= x3
    , y3 >= y4
    ]

simpleQuadris m n = [u
    | u <- quadris m n
    , simple u
    ]

printQuadri m n q = do
    putStrLn $ show q
    putStrLn $ show $ quadriAngles q
    putStrLn $ "Simple: " ++ (show $ simple q)
    putStrLn $ prettyMatrix $ matrix (m+1) (n+1) $ 
        \(i,j) -> if Point (i-1) (j-1) == p1 q then 1
                  else if Point (i-1) (j-1) == p2 q then 2
                  else if Point (i-1) (j-1) == p3 q then 3
                  else if Point (i-1) (j-1) == p4 q then 4
                  else 0

main = do
    putStrLn "Project Euler: Problem 453 - Lattice Quadriliterals"

    let q1 = Quadrilateral (Point 0 0) (Point 0 1) (Point 0 2) (Point 2 2)
    print $ quadriAngles q1

    let q = quadris 2 2
    putStrLn $ "Length: " ++ (show $ length q)

    mapM_ (printQuadri 2 2) $ q

    putStrLn $ "Length: " ++ (show $ length q)


