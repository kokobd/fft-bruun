module Isumi.Math.MatrixUtil
  ( imageToMatrix
  ) where

import           Codec.Picture
import           Numeric.LinearAlgebra (Matrix)
import qualified Numeric.LinearAlgebra as LA

-- | Convert image of Pixel8 to matrix
imageToMatrix :: Image Pixel8 -> Matrix Double
imageToMatrix =
  LA.fromColumns . fmap (LA.fromList . fmap fromIntegral) . imageToPixels

-- | Convert image to list of list of pixels, in column major order
imageToPixels :: Pixel a => Image a -> [[a]]
imageToPixels image =
  fmap (\c -> fmap (\r -> pixelAt image c r) [0.. height - 1]) [0..width - 1]
  where
  width = imageWidth image
  height = imageHeight image

