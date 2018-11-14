module Isumi.Math.FFT.Bruun.Internal
  ( fftDirect
  ) where

import           Data.Complex
import qualified Data.Vector.Unboxed  as UV
import           Numeric

import           Isumi.Math.FFT.Utils

fftDirect :: UV.Vector Double -> UV.Vector (Complex Double)
fftDirect xs = UV.generate len genEntry
  where
  len = UV.length xs
  genEntry k = UV.sum . flip UV.imap xs $ \n xn ->
      realToComplex xn
    * exp (coef * fromIntegral n * realToComplex k)
  coef = (-2) * pi * virtToComplex 1 / fromIntegral len

