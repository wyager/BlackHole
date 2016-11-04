{-# LANGUAGE Strict #-}
module Main (main) where
import Point

import qualified Codec.Picture as Pic
import qualified Codec.Picture.Png as Png

data Collision = Accretion
               | BlackHole
               | Celestial

data Config = Config {
    scale :: Double,
    blackHoleRadius :: Double,
    accretionRadius :: Double,
    celestialRadius :: Double
}

trace :: Config -> Point -> Direction -> Collision
trace (Config scale bhr ar cr) start direction = go start
    where
    direction' = scaleBy (scale / lengthOf direction) direction 
    go start 
        | accretion = Accretion
        | blackhole = BlackHole
        | celestial = Celestial
        | otherwise = go end
        where
        accretion = (y start > 0) /= (y end > 0) && (start <.> start) < (ar * ar)
        blackhole = (end <.> end) < (bhr * bhr) -- Not quite right. Can "clip through" BH
        celestial = (end <.> end) > (cr  * cr )
        end = start <+> direction'

ray :: Double -> Double -> Pic.PixelRGB8
ray x y = case coll of
    Accretion -> Pic.PixelRGB8 0xFF 0 0
    BlackHole -> Pic.PixelRGB8 0 0xFF 0
    Celestial -> Pic.PixelRGB8 0 0 0xFF
    where
    config = Config {
            scale           = 1   * 1e3,
            blackHoleRadius = 10  * 1e3,
            accretionRadius = 100 * 1e3,
            celestialRadius = 1   * 1e6
        } 
    camera = V3 0 (100 * 1e3) (500 * 1e3)
    direction = unit (scaleBy (negate 1) camera)
    right :: Point
    right = V3 (1) 0 0
    up :: Point
    up = (direction <%> right)
    screenPoint = direction <+> (scaleBy x right) <+> (scaleBy y up)
    coll = trace config camera screenPoint

pixel :: Int -> Int -> Int -> Int -> Pic.PixelRGB8
pixel h w x y = ray (xf * fov) (yf * fov)
    where
    fov = 1 / 4
    [h', w', x', y'] = map (\l -> fromIntegral l) [h,w,x,y]
    xf = (x' - w'/2) / w'
    yf = (y' - h'/2) / h'


image = Pic.generateImage (pixel h w) h w
    where
    (h,w) = (1000,1000)

main = Png.writePng "out.png" image