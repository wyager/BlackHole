{-# LANGUAGE Strict #-}
module Main (main) where
import Point

import qualified Codec.Picture as Pic
import qualified Codec.Picture.Png as Png
import qualified PerlinG as Perlin
import qualified XYZ

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
point2xyz (V3 x y z) = XYZ.XYZ x' y' z'
    where (x',y',z') = (round x, round y, round z)

stars :: Point -> Double
stars pt = if noise < 0.6 then 0 else sigmoid noise
    where 
    seed = 0x1337
    xyz = point2xyz pt
    noise = Perlin.perlin (XYZ.noise seed) (+) (*) XYZ.weight 4 xyz
    sigmoid v = 1 / (1 + exp (negate 30 * (v - 0.8))) -- NB: Perlin hash function is bad. Fix.


ray :: Double -> Double -> (Pic.PixelRGB8, Int)
ray x y = (pixel, count)
    where
    count = snd coll
    pixel = case fst coll of
        Accretion _ -> Pic.PixelRGB8 0xFF 0 0
        BlackHole _ -> Pic.PixelRGB8 0 0xFF 0
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

pixel :: Int -> Int -> Int -> Int -> Int -> (Int, Pic.PixelRGB8)
pixel h w acc x y = (acc + count, pixel)
    where
    (pixel, count) = ray (xf * fov) (yf * fov)
    fov = 1 / 6
    [h', w', x', y'] = map (\l -> fromIntegral l) [h,w,x,y]
    xf = (x' - w'/2) / w'
    yf = (y' - h'/2) / h'


(steps, image) = Pic.generateFoldImage (pixel h w) 0 h w
    where
    (h,w) = (300,300)

main = do
    Png.writePng "out.png" image
    putStrLn $ "Steps: " ++ show steps