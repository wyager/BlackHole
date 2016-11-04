{-# LANGUAGE Strict #-}
module Main (main) where
import Point

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude hiding (length)
import qualified Codec.Picture as Pic
import qualified Codec.Picture.Png as Png

data Collision = Accretion
               | BlackHole
               | Celestial

data Config = Config {
    scale :: Length Double,
    blackHoleRadius :: Length Double,
    accretionRadius :: Length Double,
    celestialRadius :: Length Double
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
        accretion = (y start > _0) /= (y end > _0) && (start <.> start) < (ar * ar)
        blackhole = (end <.> end) < (bhr * bhr) -- Not quite right. Can "clip through" BH
        celestial = (end <.> end) > (cr  * cr )
        end = start <+> direction'

ray :: Angle Double -> Angle Double -> Pic.PixelRGB8
ray ϕ θ = case coll of
    Accretion -> Pic.PixelRGB8 0xFF 0 0
    BlackHole -> Pic.PixelRGB8 0 0xFF 0
    Celestial -> Pic.PixelRGB8 0 0 0xFF
    where
    config = Config {
            scale           = 1   *~ kilo meter,
            blackHoleRadius = 10  *~ kilo meter,
            accretionRadius = 100 *~ kilo meter,
            celestialRadius = 1   *~ mega meter
        } 
    camera = V3 _0 _0 (500 *~ kilo meter)
    l = (-1) *~ meter
    direction = V3 (sin ϕ * l) (sin θ * l) (cos θ * cos ϕ * l)
    coll = trace config camera direction

pixel :: Int -> Int -> Int -> Int -> Pic.PixelRGB8
pixel h w x y = ray (xf * fov) (yf * fov)
    where
    fov = pi / _4
    [h', w', x', y'] = map (\l -> fromIntegral l *~ one) [h,w,x,y]
    xf = (x' - w'/_2) / w'
    yf = (y' - h'/_2) / h'

image = Pic.generateImage (pixel h w) h w
    where
    (h,w) = (50,50)

main = Png.writePng "out.png" image