{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, BangPatterns #-}

module XY where

import Data.Word (Word64)
import Data.Bits ((.&.), shiftL, shiftR)
import PerlinG
import Data.ByteString (ByteString)
import Data.Hashable (Hashable, hash, hashWithSalt)


data XY = XY {x :: !Word64, y :: !Word64} deriving Show

right (XY x y) = XY (x+1) y
down (XY x y) = XY x (y+1)

weight :: XY -> Word64 -> Double
weight (XY x y) space = fromIntegral x * fromIntegral y / ((fromIntegral space)^2)

noise :: Int -> Int -> XY -> Double
noise s n (XY x y) = 0.5 +  0.5 * fromIntegral w / fromIntegral (maxBound :: Int)
    where
    f :: Hashable a => a -> Int -> Int
    f = flip hashWithSalt
    w = f ("ca$h" :: ByteString) $ f x $ f y $ f s $ f n $ 0x1337

instance Perlin XY Word64 where
    {-# INLINE grid #-}
    grid n (XY x y) = Coord macro micro ratio
        where
        ratio = 1 `shiftL` n
        mask = ratio - 1
        macro = XY (x `shiftR` n) (y `shiftR` n)
        micro = XY (x .&. mask) (y .&. mask)
    {-# INLINE interpolate #-}
    interpolate noise (+) mul weightAt (Coord macro micro@(XY μx μy) ratio) = tlv + trv + blv + brv
        where
        tl = noise macro
        tr = noise (right macro)
        bl = noise (down macro)
        br = noise (down $ right macro)
        tlp = XY (ratio-μx) (ratio-μy)
        trp = XY μx (ratio-μy)
        blp = XY (ratio-μx) μy
        brp = XY μx μy
        tlv = mul (weightAt tlp ratio) tl 
        trv = mul (weightAt trp ratio) tr 
        blv = mul (weightAt blp ratio) bl 
        brv = mul (weightAt brp ratio) br 