-- NB: Borrowed from my own project, GeoLabel
{-# LANGUAGE TypeFamilies, Strict, DeriveFunctor #-}
module Point (
    V3(..), (<+>), (<->), (<.>), (<%>), scaleBy, kick, unit, lengthOf, x, y, z,
    Point,
    Bivector,
    Direction
) where


-- | A vector of three things.
data V3 a = V3 a a a deriving (Show, Functor)
-- | A vector in R³.
type Point = V3 Double
-- | A bivector for R³.
-- Turns out cross products are super weird. 
-- https://en.wikipedia.org/wiki/Exterior_algebra
type Bivector = V3 Double

type Direction = Point

-- | Vector Addition
(V3 a b c) <+> (V3 x y z) = V3 (a + x) (b + y) (c + z)
-- | Vector Subtraction
(V3 a b c) <-> (V3 x y z) = V3 (a - x) (b - y) (c - z)
-- | Dot Product
(V3 a b c) <.> (V3 x y z) = (a * x) + (b * y) + (c * z)
-- | Cross Product
(V3 a b c) <%> (V3 x y z) = V3 (b*z - c*y) (c*x - a*z) (a*y - b*x)

x :: Point -> Double
x (V3 x y z) = x

y :: Point -> Double
y (V3 x y z) = y

z :: Point -> Double
z (V3 x y z) = z

scaleBy :: Double -> Point -> Point
scaleBy s (V3 a b c) = V3 (s * a) (s * b) (s * c)

kick :: Point -> Point -> Point
kick point destination = point <+> delta
    where
    delta = small (destination <-> point)

small :: Point -> Point
small vector = scaleBy (0.01) (unit vector) -- 1 centimeter

unit :: Point -> Point
unit vector = scaleBy (1.0 / lengthOf vector) vector

lengthOf :: Point -> Double
lengthOf vector = sqrt (vector <.> vector)