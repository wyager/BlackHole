-- NB: Borrowed from my own project, GeoLabel
module Polar (Polar(..)) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude

data Polar = Polar { r :: Length Double, 
                     theta :: Angle Double, 
                     phi :: Angle Double } deriving Show
