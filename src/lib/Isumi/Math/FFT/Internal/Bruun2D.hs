module Isumi.Math.FFT.Internal.Bruun2D
  ( fft2dBruun
  ) where

import           Data.Complex
import qualified Data.Vector.Generic           as GV
import           Isumi.Math.FFT.Internal.Bruun (fftBruun, fftBruunC)
import qualified Numeric.LinearAlgebra         as LA
import           Numeric.LinearAlgebra.Data    (Matrix)

fft2dBruun :: Matrix Double -> Maybe (Matrix (Complex Double))
fft2dBruun m = do
  rows' <- traverse (fftBruun . GV.convert) (LA.toRows m)
  cols' <- traverse (fftBruunC . GV.convert) . LA.toColumns . LA.fromRows
           $ fmap GV.convert rows'
  pure $ LA.fromColumns (fmap GV.convert cols')

