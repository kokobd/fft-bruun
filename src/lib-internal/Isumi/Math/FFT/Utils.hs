module Isumi.Math.FFT.Utils
  ( realToComplex
  , virtToComplex
  ) where

import           Data.Complex

realToComplex :: (Real a, Fractional b) => a -> Complex b
realToComplex x = realToFrac x :+ 0

virtToComplex :: (Real a, Fractional b) => a -> Complex b
virtToComplex y = 0 :+ realToFrac y

