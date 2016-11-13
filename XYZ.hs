{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, DeriveFunctor, BangPatterns #-}

module XYZ where

import Data.Word (Word64)
import Data.Bits ((.&.), shiftL, shiftR)
import PerlinG
import Data.ByteString (ByteString)
-- import Data.Hashable (Hashable, hash, hashWithSalt)
import Crypto.MAC.SipHash (SipKey(..), SipHash(..), hash)
import Data.Word (Word64)


data XYZ = XYZ {x :: !Word64, y :: !Word64, z :: !Word64} deriving Show

data Eight a = Eight a a a a a a a a deriving (Show, Functor)

neighbors :: XYZ -> Eight XYZ
neighbors (XYZ x y z) = Eight
    (XYZ (x)   (y)   (z)  )
    (XYZ (x)   (y)   (z+1))
    (XYZ (x)   (y+1) (z)  )
    (XYZ (x)   (y+1) (z+1))
    (XYZ (x+1) (y)   (z)  )
    (XYZ (x+1) (y)   (z+1))
    (XYZ (x+1) (y+1) (z)  )
    (XYZ (x+1) (y+1) (z+1))

microsubs :: Word64 -> XYZ -> Eight XYZ
microsubs ratio (XYZ x y z) = Eight
    (XYZ (ratio-x) (ratio-y) (ratio-z))
    (XYZ (ratio-x) (ratio-y) (      z))
    (XYZ (ratio-x) (      y) (ratio-z))
    (XYZ (ratio-x) (      y) (      z))
    (XYZ (      x) (ratio-y) (ratio-z))
    (XYZ (      x) (ratio-y) (      z))
    (XYZ (      x) (      y) (ratio-z))
    (XYZ (      x) (      y) (      z))

zip8 :: (a -> b -> c) -> Eight a -> Eight b -> Eight c
zip8 f (Eight a1 a2 a3 a4 a5 a6 a7 a8) (Eight b1 b2 b3 b4 b5 b6 b7 b8) = Eight
    (f a1 b1)
    (f a2 b2)
    (f a3 b3)
    (f a4 b4)
    (f a5 b5)
    (f a6 b6)
    (f a7 b7)
    (f a8 b8)

fold8 :: (a -> a -> a) -> Eight a -> a
fold8 (+) (Eight a b c d e f g h) = a + b + c + d + e + f + g + h

weight :: XYZ -> Word64 -> Double
weight (XYZ x y z) space = fromIntegral x * fromIntegral y * fromIntegral z / ((fromIntegral space)^3)

noise :: Int -> Int -> XYZ -> Double
noise s n (XYZ x y z) = fromIntegral h4 / fromIntegral (maxBound :: Word64)
    where
    f = fromIntegral 
    SipHash h1 = hash (SipKey x y) "a"
    SipHash h2 = hash (SipKey (f s) (f n)) "b"
    SipHash h3 = hash (SipKey h1 h2) "c"
    SipHash h4 = hash (SipKey h3 z)  "d"

    -- f :: Hashable a => a -> Int -> Int
    -- f = flip hashWithSalt
    -- w = f ("ca$h" :: ByteString) $ f x $ f y $ f z $ f s $ f n $ hash ("dollaz" :: ByteString)

instance Perlin XYZ Word64 where
    {-# INLINE grid #-}
    grid n (XYZ x y z) = Coord macro micro ratio
        where
        ratio = 1 `shiftL` n
        mask = ratio - 1
        macro = XYZ (x `shiftR` n) (y `shiftR` n) (z `shiftR` n)
        micro = XYZ (x .&. mask) (y .&. mask) (z .&. mask)
    {-# INLINE interpolate #-}
    interpolate noise sum mul weightAt (Coord macro micro ratio) = fold8 sum vals
        where
        noises = fmap noise (neighbors macro)
        weight point = weightAt point ratio
        points = microsubs ratio micro
        vals = zip8 mul (fmap weight points) noises

