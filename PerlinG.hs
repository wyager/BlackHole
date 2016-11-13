{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, BangPatterns #-}
module PerlinG where

import Data.Vector as V

data Coord point space = Coord {macro :: !point, micro :: !point, ratio :: !space}


class Perlin point space | point -> space where
    grid :: Int -> point -> Coord point space
    interpolate :: (point -> noise) -> (output -> output -> output) -> (weight -> noise -> output) -> (point -> space -> weight) -> Coord point space -> output



{-# INLINE perlins #-}
perlins :: Perlin point space
        => (Int -> point -> noise) -- Noise function with seed
        -> (output -> output -> output) -- Add noise
        -> (weight -> noise -> output) -- Multiply noise by a weight
        -> (point -> space -> weight) -- Weight based on proximity
        -> Int -- Number of frequency levels, starting from highest
        -> point
        -> Vector output
perlins noise add mul weightAt levels point = V.generate levels noiseAt
    where
    noiseAt level = interpolate (noise level) add mul weightAt (grid level point)

{-# INLINE perlin #-}
perlin :: (Perlin point space, Fractional output)
       => (Int -> point -> noise) -- Noise function with seed
       -> (output -> output -> output) -- Add noise
       -> (weight -> noise -> output) -- Multiply noise by a weight
       -> (point -> space -> weight) -- Weight based on proximity
       -> Int -- Number of frequency levels, starting from highest
       -> point
       -> output
perlin noise add mul weightAt levels point = normalizedNoise
    where
    weights = V.reverse $ V.iterateN levels (/2) 1
    rawNoise = perlins noise add mul weightAt levels point
    scaledNoise = V.zipWith (*) weights rawNoise
    normalizedNoise = V.sum scaledNoise / V.sum weights
