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
import qualified Debug.Trace as Debug
import Data.Word (Word64)
import System.Environment (getArgs)

gravity = True
debugTextures = False

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
    accretionRadius :: Double,
    celestialRadius :: Double,
    accretionWidth :: Double -- Falls off as exp(-(h/width)^2)
}

-- Black hole radius
bhr :: Double
bhr = 1.0

bhr² :: Double
bhr² = 1.0

trace :: Config -> Point -> Direction -> (Color)
trace cfg@(Config scale ar cr acrw) start direction = go (Light 1 0 0 0) start 0 dir
    where
    dir = unit direction
    -- Looks like this is constant. Weird.
    h = start <%> dir
    h² = h <.> h
    accretionLimit = acrw * 3
    go !light !start !n !direction
        | opacity light <= 0.01 = toColor light
        -- | Just isect <- accretionI = go (opacity * 0.6) (color <+> accretionColor cfg isect) end (n+1)
        | blackhole = toColor (light' `plus` blackholeLight start)
        | celestial = toColor (light' `plus` celestialLight end)
        | otherwise = go light' end (if n > lengthOf direction then n else lengthOf direction) direction'
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
            | (start <.> start) < bhr² = True -- We inside 
            | closestSquared < bhr² = True -- Inside at some point
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
        startDistance = lengthOf start
        -- Dynamically decrease scale near black hole.
        -- You can graph the sigmoid curve to get a feel for what it's doing.
        sa :: Double
        sa = 10 -- Scaling aggressiveness factor
        blackholeStep :: Double
        blackholeStep = (1/) $ (+1) $ exp $ (*sa) $ ((4/sa)+) $ (1+) $ negate $ startDistance
        accretionStep :: Double
        accretionStep = (1/) $ (+1) $ exp $ (*sa) $ ((4/sa)+) $ negate $ (/20) $ (/acrw) $ abs $ y start
        adjustedStep :: Double
        adjustedStep = min blackholeStep accretionStep / lengthOf direction -- Account for closeness to objects and the accrued psuedo-velocity
        adjustedScale :: Double
        adjustedScale = if startDistance > 40 * bhr
            then 10 -- Gotta go fast
            else scale * adjustedStep
        -- leapfrog
        end = start <+> (scaleBy adjustedScale direction)
        direction' :: V3 Double
        direction' = direction <+> scaleBy adjustedScale acceleration -- Apparently we shouldn't renormalize
        acceleration :: V3 Double
        acceleration = if gravity
            then scaleBy (-1.5 * h² / ((end <.> end) ** 2.5)) end
            else V3 0 0 0


point2xyz :: Point -> XYZ.XYZ
point2xyz pt = XYZ.XYZ x y z
    where 
    center point = point + (maxBound `div` 2 :: Word64)
    V3 x y z = fmap (center . round) pt


noiseWith :: Int -> Point -> Double
noiseWith seed = Perlin.perlin (XYZ.noise seed) (+) (*) XYZ.weight 5 . point2xyz

celestialLight :: Point -> Light
celestialLight pt@(V3 x y z) = if debugTextures
        then Light 0 1 0 0
        else Light 0 brightness brightness brightness
    where 
    brightness = if noise < 0.70 then 0 else 4 * sigmoid noise
    seed = 0x1337
    noise = noiseWith seed $ fmap (*3) pt -- *100
    sigmoid v = 1 / (1 + exp (negate 30 * (v - 0.8)))

parity :: Point -> Bool
parity (V3 x y z) = even (round x + round y + round z)

accretionLight :: Config -> Point -> Double -> Light
accretionLight config pt@(V3 x y z) len = if debugTextures
    then if parity pt
        then Light 0 0 0 1
        else Light 0 0 1 0
    else scaleLight (farFalloff * nearFalloff * widthFalloff * len / accretionWidth config) rawLight
    where
    w = y / width
    farFalloff = (1/) $ (1+) $ exp $ (*15) $ (subtract 0.6) $ (/ accretionRadius config) $ r
    nearFalloff = (1/) $ (1+) $ exp $ (*10) $ (+1.7) $ negate $ r
    widthFalloff = exp $ negate $ w * w
    rawLight = RGB.uncurryRGB (Light 0.8) rgb
    l = (lengthOf pt - bhr) / (accretionRadius config - bhr)
    rgb = HSV.hsv (50-l*30) 0.8 1
    -- noiseY = noiseWith 0xBEEF $ fmap (/1e2) pt
    noise = (4*) $ noiseWith 0xCAFE $ fmap (200*) pt
    -- scrambled = V3 (x + noiseX*1000) (y + noiseY*1000) z
    r = lengthOf pt
    oscillation = (0.4*) $ (^3) $ sin $ (+ noise) $ ((25*r)+) $ atan2 x z
    width = (oscillation + 0.6) * accretionWidth config
    -- oscillation = (0.2*) $ sin $ (/400) $ lengthOf scrambled

grid (V3 x y z) = ((sin x + sin y + sin z) + 3)/6

blackholeLight :: Point -> Light
blackholeLight pt@(V3 x y z) = if debugTextures
    then let c = grid (scaleBy (4*pi) pt) in Light 0 c c c
    else Light 0 0 0 0 -- Light 0 b b b
    -- where
    -- phi   = atan2 x z
    -- theta = atan2 x y
    -- b = (/2) $ (1+) $ sin $ (*10) $ x + y + z

ray :: Double -> Double -> Color
ray x y = trace config camera screenPoint
    where
    config = Config {
            scale           = 0.1,
            accretionRadius = 5,
            celestialRadius = 2000,
            accretionWidth  = 0.01
        } 
    camera = V3 0 5 25 
    -- NB: Putting the camera closer to the black hole
    -- decreases the relative angular size of the "ring" effect
    direction = unit (scaleBy (negate 1) camera)
    right :: Point
    right = V3 (1) 0 0
    up :: Point
    up = (direction <%> right)
    screenPoint = direction <+> (scaleBy x right) <+> (scaleBy y up)

pixel :: ImageRange -> Int -> Int -> Color
pixel (ImageRange w h fov xoff yoff) x y = ray (xoff + xf * fov) (yoff + yf * fov)
    where
    [h', w', x', y'] = map (\l -> fromIntegral l) [h,w,x,y]
    xf = xoff + (x' - w'/2) / h' -- We actually want these to be the same to avoid stretching
    yf = yoff + (y' - h'/2) / h'

data ImageRange = ImageRange {
    w :: Int,
    h :: Int,
    fov :: Double,
    xoff :: Double,
    yoff :: Double
}

pixels :: ImageRange -> Vec.Vector (Vec.Vector Pic.PixelRGB8)
pixels range = pixels
    where
    map2 f = Vec.map (Vec.map f)
    colors = Strat.using 
        (Vec.generate (w range) $ \x -> Vec.generate (h range) $ \y -> pixel range x y)
        (Strat.parVector 4) -- Generate 16 columns at a time in parallel
    amplitude (V3 r g b) = max r (max g b)
    amplitudes = map2 amplitude colors 
    maxAmplitude = Vec.maximum (Vec.map Vec.maximum amplitudes)
    scaled = map2 (fmap (/maxAmplitude)) colors
    word8s = map2 (fmap (round . (*255))) scaled
    pixels = map2 (\(V3 r g b) -> Pic.PixelRGB8 r g b) word8s


main = do
    args <- getArgs
    let (w,h,fov,xoff,yoff) = case args of
            ["-h"] -> do
                putStrLn "BlackHole options:"
                putStrLn "./BlackHole # Runs with default options (200x200, fov=1, no offset)"
                putStrLn "./BlackHole w h # Runs with the specified with, height. fov = 1, no offset"
                putStrLn "./BlackHole w h fov # Runs with the specified with, height, fov. no offset"
                putStrLn "./BlackHole w h fov xoff yoff # Runs with the specified with, height, fov, x offset, y offset"
            [w,h] -> (read w, read h, 1, 0, 0)
            [w,h,fov] -> (read w, read h, read fov, 0, 0)
            [w,h,fov,xoff,yoff] -> (read w, read h, read fov, read xoff, read yoff)
            _ -> (200,100,1,0,0)
    let array = pixels (ImageRange w h fov xoff yoff)
    let image = Pic.generateImage (\x y -> (array Vec.! x) Vec.! y) w h
    Png.writePng "out.png" image


