module Isumi.Math.FFT.Internal.Bruun
  ( fftDirect
  )
where

import           Data.Complex
import qualified Data.Vector.Unboxed           as UV

import           Isumi.Math.FFT.Internal.Utils

fftDirect :: UV.Vector Double -> UV.Vector (Complex Double)
fftDirect xs = UV.generate len genEntry
 where
  len = UV.length xs
  genEntry k = UV.sum . flip UV.imap xs $ \n xn ->
    realToComplex xn * exp (coef * fromIntegral n * realToComplex k)
  coef = (-2) * pi * compI / fromIntegral len

{-|
  Computes the complex roots of unity
-}
cruN :: Int -> UV.Vector (Complex Double)
cruN n = UV.generate n $ \k ->
  exp (realToComplex (2 * k) * pi * compI) / fromIntegral n

