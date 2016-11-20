{-# LANGUAGE TypeOperators, BangPatterns #-}
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
import System.Environment (getArgs)

data Light = Light {
    opacity :: !Double,
    r :: !Double,
    g :: !Double,
    b :: !Double }

plus :: Light -> Light -> Light
plus !(Light oi ri gi bi) !(Light on rn gn bn) = Light o' r' g' b'
    where
    o' = oi * on
    r' = ri + (rn*oi)
    g' = gi + (gn*oi)
    b' = bi + (bn*oi)

scaleLight :: Double -> Light -> Light
scaleLight l (Light o r g b) = Light (1-(l*(1-o))) (r*l) (g*l) (b*l)

toColor :: Light -> Color
toColor (Light o r g b) = V3 r g b


type Color = V3 Double

data Config = Config {
    scale :: Double,
    blackHoleRadius :: Double,
    accretionRadius :: Double,
    celestialRadius :: Double,
    accretionWidth :: Double -- Falls off as exp(-(h/width)^2)
}

trace :: Config -> Point -> Direction -> (Color, Int)
trace cfg@(Config scale bhr ar cr acrw) start direction = go (Light 1 0 0 0) start 0 
    where
    accretionLimit = acrw * 3
    direction' = unit direction 
    go !light !start !n
        | opacity light <= 0.01 = (toColor light, n)
        -- | Just isect <- accretionI = go (opacity * 0.6) (color <+> accretionColor cfg isect) end (n+1)
        | blackhole = (toColor (light' `plus` blackholeLight start), n)
        | celestial = (toColor (light' `plus` celestialLight end), n)
        | otherwise = go light' end (n+1)
        where
        light'
            | abs (y start) > accretionLimit = light -- y too big
            | startDistance > ar = light -- r too big
            | otherwise = light `plus` accretionLight cfg start adjustedScale
        -- Finds the point in the middle that hits the disk (y = 0)
        -- accretionI 
        --     | (y start > 0) == (y end > 0) = Nothing -- Do we cross the disk plane?
        --     | crossSquared < (ar * ar) && crossSquared > (bhr * bhr) = Just accretionCross -- Is the crossing within the disk's radius?
        --     | otherwise = Nothing
        --     where
        --         -- Parallel to disk?
        --         accretionCross = if -1e-5 < distance && distance < 1e-5
        --             then scaleBy 0.5 start <+> scaleBy 0.5 end
        --             else scaleBy c_start start <+> scaleBy c_end end
        --         crossSquared = accretionCross <.> accretionCross
        --         y_start = y start
        --         y_end = y end
        --         distance = y_start - y_end
        --         c_start = abs (y_end / distance)
        --         c_end = 1 - c_start
        -- Does the ray hit the black hole?
        blackhole
            | startDistance > (bhr + adjustedScale) = False -- No way Jose
            | (start <.> start) < (bhr * bhr) = True -- We inside 
            | closestSquared < (bhr * bhr) = True -- Inside at some point
            | otherwise = False -- Nearby, but not inside
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
        -- Dynamically decrease scale near black hole.
        -- You can graph the sigmoid curve to get a feel for what it's doing.
        sa :: Double
        sa = 10 -- Scaling aggressiveness factor
        blackholeScale :: Double
        blackholeScale = (scale*) $ (1/) $ (+1) $ exp $ (*sa) $ ((4/sa)+) $ (1+) $ negate $ (/bhr) $ startDistance
        accretionScale :: Double
        accretionScale = (scale*) $ (1/) $ (+1) $ exp $ (*sa) $ ((4/sa)+) $ negate $ (/20) $ (/acrw) $ abs $ y start
        adjustedScale :: Double
        adjustedScale = min blackholeScale accretionScale


point2xyz :: Point -> XYZ.XYZ
point2xyz pt = XYZ.XYZ x y z
    where 
    center point = point + (maxBound `div` 2 :: Word64)
    V3 x y z = fmap (center . round) pt


noiseWith :: Int -> Point -> Double
noiseWith seed = Perlin.perlin (XYZ.noise seed) (+) (*) XYZ.weight 5 . point2xyz

celestialLight :: Point -> Light
celestialLight pt = Light 0 brightness brightness brightness
    where 
    brightness = if noise < 0.70 then 0 else sigmoid noise
    seed = 0x1337
    noise = noiseWith seed $ fmap (/1e2) pt
    sigmoid v = 1 / (1 + exp (negate 30 * (v - 0.8)))

accretionLight :: Config -> Point -> Double -> Light
accretionLight config pt@(V3 x y z) len = scaleLight (farFalloff * nearFalloff * widthFalloff * len / accretionWidth config) rawLight
    where
    w = y / width
    farFalloff = (1/) $ (1+) $ exp $ (*10) $ (subtract 0.8) $ (/ accretionRadius config) $ r
    nearFalloff = (1/) $ (1+) $ exp $ (*10) $ (+1.4) $ negate $ (/ blackHoleRadius config) $ r
    widthFalloff = exp $ negate $ w * w
    rawLight = RGB.uncurryRGB (Light 0.8) rgb
    l = (lengthOf pt - blackHoleRadius config) / (accretionRadius config - blackHoleRadius config)
    rgb = HSV.hsv (50-l*30) 1 1
    -- noiseY = noiseWith 0xBEEF $ fmap (/1e2) pt
    noise = (8*) $ noiseWith 0xCAFE $ fmap (/50) pt
    -- scrambled = V3 (x + noiseX*1000) (y + noiseY*1000) z
    r = lengthOf pt
    oscillation = (0.4*) $ sin $ (+ noise) $ ((r/200)+) $ (2*pi*) $ atan2 x z
    width = (oscillation + 0.6) * accretionWidth config
    -- oscillation = (0.2*) $ sin $ (/400) $ lengthOf scrambled

blackholeLight :: Point -> Light
blackholeLight pt@(V3 x y z) = Light 0 b b b
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
            celestialRadius = 1  * 1e6,
            accretionWidth  = 1  * 1e2
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


w = 200
h = 100




main = do
    args <- getArgs
    let (w,h) = case args of
            [w,h] -> (read w, read h)
            _ -> (200,100)
    let (steps, array) = pixels w h
    let image = Pic.generateImage (\x y -> (array Vec.! x) Vec.! y) w h
    Png.writePng "out.png" image
    putStrLn $ "Steps: " ++ show steps


