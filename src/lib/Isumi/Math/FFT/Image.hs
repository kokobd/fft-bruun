module Isumi.Math.FFT.Image
  ( fftWithImage
  ) where

import           Codec.Picture         (Image, Pixel8)
import           Data.Complex
import           Isumi.Math.FFT.Bruun  (fft2dBruun)
import           Isumi.Math.MatrixUtil (imageToMatrix)
import           Numeric.LinearAlgebra (Matrix)

fftWithImage :: Image Pixel8 -> Maybe (Matrix (Complex Double))
fftWithImage = fft2dBruun . imageToMatrix
