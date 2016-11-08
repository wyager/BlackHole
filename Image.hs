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
weights = normalize $ V.reverse $ V.iterateN 5 (/2) 1

pixel :: Int -> Int -> Pic.PixelRGB8
pixel x y = Pic.PixelRGB8 (chr r) (chr g) (chr b)
    where
    chr val = round (val*255)
    x' = fromIntegral x
    y' = fromIntegral y
    r = perlin' 1 xy -- (+0.5) $ (/2) $ sin $ 0.1 * (x' + y' + 300 * perlin' 1 xy)
    g = perlin' 1 xy -- (+0.5) $ (/2) $ sin $ 0.1 * (x' + y' + 300 * perlin' 1 xy)
    b = perlin' 1 xy -- (+0.5) $ (/2) $ sin $ 0.1 * (x' + y' + 300 * perlin' 1 xy)
    perlin' seed xy = V.sum $ V.zipWith (*) weights $ perlins (XYZ.noise seed) (+) (*) XYZ.weight (V.length weights) xy
    xy = XYZ.XYZ (fromIntegral x) (fromIntegral y) 0

pic = Pic.generateImage pixel 100 100

main = Png.writePng "out.png" pic