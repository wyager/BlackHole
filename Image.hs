-- import Perlin
import Codec.Picture as Pic
import qualified Data.Vector as V
import Codec.Picture.Png as Png
import PerlinG
import qualified XY
import qualified XYZ
import Data.Monoid (Sum, getSum)

normalize vec = V.map (/ V.sum vec) vec

weights :: V.Vector Double
weights = normalize $ V.reverse $ V.iterateN 5 (*0.95) 1

pixel :: Int -> Int -> Pic.PixelRGB8
pixel x y = Pic.PixelRGB8 (chr r) (chr g) (chr b)
    where
    chr val = round (val*255)
    x' = fromIntegral x
    y' = fromIntegral y
    v = perlin' 1 xy
    v2 = (v^10)*10 + 0.2*v
    v3 = if v2 > 1 then 1 else v2
    (r,g,b) = (v3,v3,v3)
    -- r = perlin' 1 xy -- (+0.5) $ (/2) $ sin $ 0.1 * (x' + y' + 300 * perlin' 1 xy)
    -- g = perlin' 1 xy -- (+0.5) $ (/2) $ sin $ 0.1 * (x' + y' + 300 * perlin' 1 xy)
    -- b = perlin' 1 xy -- (+0.5) $ (/2) $ sin $ 0.1 * (x' + y' + 300 * perlin' 1 xy)
    perlin' seed xy = V.sum $ V.zipWith (*) weights $ perlins (XY.noise seed) (+) (*) XY.weight (V.length weights) xy
    xy = XY.XY (fromIntegral x) (fromIntegral y)

pic = Pic.generateImage pixel 1000 1000

main = Png.writePng "out.png" pic