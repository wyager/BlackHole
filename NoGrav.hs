{-# LANGUAGE Strict, TypeOperators #-}
module Main (main) where
import Point

import qualified Codec.Picture as Pic
import qualified Codec.Picture.Png as Png
import qualified PerlinG as Perlin
import qualified XYZ
import qualified XY
import Data.Array.Repa ((:.)((:.)), (!), Z(Z))
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Repr.Vector (V)
import qualified Control.Monad.Identity as Identity
import Data.Word (Word64)

data Collision = Accretion Point
               | BlackHole Point
               | Celestial Point

data Config = Config {
    scale :: Double,
    blackHoleRadius :: Double,
    accretionRadius :: Double,
    celestialRadius :: Double
}

trace :: Config -> Point -> Direction -> (Collision, Int)
trace (Config scale bhr ar cr) start direction = go start 0 
    where
    direction' = scaleBy (scale / lengthOf direction) direction 
    go start n
        | accretion = (Accretion end, n)
        | blackhole = (BlackHole end, n)
        | celestial = (Celestial end, n)
        | otherwise = go end (n+1)
        where
        accretion = (y start > 0) /= (y end > 0) && (start <.> start) < (ar * ar)
        blackhole = (end <.> end) < (bhr * bhr) -- Not quite right. Can "clip through" BH
        celestial = (end <.> end) > (cr  * cr )
        end = start <+> direction'


point2xyz :: Point -> XYZ.XYZ
point2xyz pt = XYZ.XYZ x y z
    where 
    center point = point + (maxBound `div` 2 :: Word64)
    V3 x y z = fmap (center . round) pt


noiseWith :: Int -> Point -> Double
noiseWith seed = Perlin.perlin (XYZ.noise seed) (+) (*) XYZ.weight 4 . point2xyz

stars :: Point -> Double
stars pt = if noise < 0.6 then 0 else sigmoid noise
    where 
    seed = 0x1337
    noise = noiseWith seed $ fmap (/3e2) pt
    sigmoid v = 1 / (1 + exp (negate 30 * (v - 0.8)))

accretion :: Point -> Double
accretion pt = noise
    where
    seed = 0xBEEF
    noise = noiseWith seed $ fmap (/1e2) pt


ray :: Double -> Double -> (Pic.PixelRGB8, Int)
ray x y = (pixel, count)
    where
    count = snd coll
    pixel = case fst coll of
        Accretion pt -> Pic.PixelRGB8 brightness brightness 0
            where
            brightness = round (255 * accretion pt)
        BlackHole pt -> Pic.PixelRGB8 0 0xFF 0
        Celestial pt -> Pic.PixelRGB8 brightness brightness brightness
            where brightness = round (255 * stars pt)
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
    coll = trace config camera screenPoint

pixel :: Int -> Int -> Int -> Int -> (Int, Pic.PixelRGB8)
pixel w h x y = (count, pixel)
    where
    (pixel, count) = ray (xf * fov) (yf * fov)
    fov = 1 / 6
    [h', w', x', y'] = map (\l -> fromIntegral l) [h,w,x,y]
    xf = (x' - w'/2) / w'
    yf = (y' - h'/2) / h'

pixels :: Int -> Int -> (Int, Repa.Array V (Z :. Int :. Int) Pic.PixelRGB8)
pixels w h = (steps, pixels)
    where
    image = Repa.fromFunction (Z :. w :. h) 
          $ \(Z :. x :. y) -> pixel w h x y
    result :: Repa.Array V (Z :. Int :. Int) (Int, Pic.PixelRGB8)
    result = Identity.runIdentity $ Repa.computeP image
    pixels = Identity.runIdentity $ Repa.computeP $ Repa.map snd result
    steps = Identity.runIdentity $ Repa.foldAllP (+) 0 $ Repa.map fst result



w = 300
h = 300
(steps, array) = pixels w h
image = Pic.generateImage (\x y -> array ! (Z :. x :. y)) w h


main = do
    Png.writePng "out.png" image
    putStrLn $ "Steps: " ++ show steps


