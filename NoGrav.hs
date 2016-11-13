{-# LANGUAGE TypeOperators #-}
module Main (main) where
import Point

import qualified Codec.Picture as Pic
import qualified Codec.Picture.Png as Png
import qualified PerlinG as Perlin
import qualified XYZ
import qualified XY
import qualified Data.Vector as Vec
import qualified Control.Monad.Identity as Identity
import qualified Data.Colour.RGBSpace as RGB
import qualified Data.Colour.RGBSpace.HSV as HSV
import qualified Data.Vector.Strategies as Strat
import Data.Word (Word64)

data Collision = Accretion Point
               | BlackHole Point
               | Celestial Point

type Color = V3 Double

data Config = Config {
    scale :: Double,
    blackHoleRadius :: Double,
    accretionRadius :: Double,
    celestialRadius :: Double
}

trace :: Config -> Point -> Direction -> (Color, Int)
trace cfg@(Config scale bhr ar cr) start direction = go 1 (V3 0 0 0) start 0 
    where
    direction' = unit direction 
    go opacity color start n
        | opacity <= 0.01 = (color, n)
        | Just isect <- accretionI = go (opacity * 0.6) (color <+> accretionColor cfg isect) end (n+1)
        | Just isect <- blackholeI = (color <+> blackholeColor isect, n)
        | celestial = (color <+> celestialColor end, n)
        | otherwise = go opacity color end (n+1)
        where
        -- Finds the point in the middle that hits the disk (y = 0)
        accretionI 
            | (y start > 0) == (y end > 0) = Nothing -- Do we cross the disk plane?
            | crossSquared < (ar * ar) && crossSquared > (bhr * bhr) = Just accretionCross -- Is the crossing within the disk's radius?
            | otherwise = Nothing
            where
                -- Parallel to disk?
                accretionCross = if -1e-5 < distance && distance < 1e-5
                    then scaleBy 0.5 start <+> scaleBy 0.5 end
                    else scaleBy c_start start <+> scaleBy c_end end
                crossSquared = accretionCross <.> accretionCross
                y_start = y start
                y_end = y end
                distance = y_start - y_end
                c_start = abs (y_end / distance)
                c_end = 1 - c_start
        -- Does the ray hit the black hole?
        blackholeI
            | startDistance > (bhr + adjustedScale) = Nothing -- No way Jose
            | (start <.> start) < (bhr * bhr) = Just start -- We inside 
            | closestSquared < (bhr * bhr) = Just start -- Inside at some point
            | otherwise = Nothing -- Nearby, but not inside
            where
            sq :: Point -> Double
            sq v = v <.> v
            numerator :: Double
            numerator = sq start * (adjustedScale ^ 2) - ((start <.> (end <-> start))^2)
            denominator :: Double
            denominator = adjustedScale ^ 2
            closestSquared :: Double
            closestSquared =  numerator /  denominator
        celestial = (end <.> end) > (cr  * cr )
        end = start <+> (scaleBy adjustedScale direction')
        startDistance = lengthOf start
        -- Dynamically decrease scale near black hole
        adjustedScale :: Double
        adjustedScale = (1000*) $ (1/) $ (+1) $ exp $ negate $ subtract 3 $ (/bhr) $ startDistance


point2xyz :: Point -> XYZ.XYZ
point2xyz pt = XYZ.XYZ x y z
    where 
    center point = point + (maxBound `div` 2 :: Word64)
    V3 x y z = fmap (center . round) pt


noiseWith :: Int -> Point -> Double
noiseWith seed = Perlin.perlin (XYZ.noise seed) (+) (*) XYZ.weight 4 . point2xyz

celestialColor :: Point -> Color
celestialColor pt = V3 brightness brightness brightness
    where 
    brightness = if noise < 0.70 then 0 else sigmoid noise
    seed = 0x1337
    noise = noiseWith seed $ fmap (/1e2) pt
    sigmoid v = 1 / (1 + exp (negate 30 * (v - 0.8)))

accretionColor :: Config -> Point -> Color
accretionColor config pt@(V3 x y z) = RGB.uncurryRGB V3 rgb
    where
    l = (lengthOf pt - blackHoleRadius config) / (accretionRadius config - blackHoleRadius config)
    rgb = HSV.hsv (30-l*30) 1 (0.8 + oscillation)
    noiseY = noiseWith 0xBEEF $ fmap (/1e2) pt
    noiseX = noiseWith 0xCAFE $ fmap (/1e2) pt
    scrambled = V3 (x + noiseX*1000) (y + noiseY*1000) z
    oscillation = (0.2*) $ sin $ (/400) $ lengthOf scrambled

blackholeColor :: Point -> Color
blackholeColor pt@(V3 x y z) = V3 b b b
    where
    phi   = atan2 x z
    theta = atan2 x y
    b = (/2) $ (1+) $ sin $ (/1e3) $ x + y + z

ray :: Double -> Double -> (Color, Int)
ray x y = (color, count)
    where
    config = Config {
            scale           = 1  * 1e3,
            blackHoleRadius = 10 * 1e3,
            accretionRadius = 30 * 1e3,
            celestialRadius = 1  * 1e6
        } 
    camera = V3 0 (100 * 1e3) (500 * 1e3)
    direction = unit (scaleBy (negate 1) camera)
    right :: Point
    right = V3 (1) 0 0
    up :: Point
    up = (direction <%> right)
    screenPoint = direction <+> (scaleBy x right) <+> (scaleBy y up)
    (color, count) = trace config camera screenPoint

pixel :: Int -> Int -> Int -> Int -> (Int, Color)
pixel w h x y = (count, pixel)
    where
    (pixel, count) = ray (xf * fov) (yf * fov)
    fov = 1/20 -- 1 / 12
    [h', w', x', y'] = map (\l -> fromIntegral l) [h,w,x,y]
    xf = (x' - w'/2) / h' -- We actually want these to be the same to avoid stretching
    yf = (y' - h'/2) / h'

pixels :: Int -> Int -> (Int, Vec.Vector (Vec.Vector Pic.PixelRGB8))
pixels w h = (count, pixels)
    where
    map2 f = Vec.map (Vec.map f)
    results = Strat.using 
        (Vec.generate w $ \x -> Vec.generate h $ \y -> pixel w h x y)
        (Strat.parVector 16) -- Generate 16 columns at a time in parallel
    counts = map2 fst results
    count = Vec.sum (Vec.map Vec.sum counts)
    colors = map2 snd results
    amplitude (V3 r g b) = max r (max g b)
    amplitudes = map2 amplitude colors 
    maxAmplitude = Vec.maximum (Vec.map Vec.maximum amplitudes)
    scaled = map2 (fmap (/maxAmplitude)) colors
    word8s = map2 (fmap (round . (*255))) scaled
    pixels = map2 (\(V3 r g b) -> Pic.PixelRGB8 r g b) word8s


w = 100
h = 50
(steps, array) = pixels w h
image = Pic.generateImage (\x y -> (array Vec.! x) Vec.! y) w h


main = do
    Png.writePng "out.png" image
    putStrLn $ "Steps: " ++ show steps


