module Isumi.Math.FFT.MatLab
  ( fftMatLab
  ) where

import           Data.Complex
import qualified Data.Vector.Generic  as GV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed  as UV
import           Foreign
import           Foreign.C
import           System.IO.Unsafe     (unsafePerformIO)

foreign import ccall unsafe "isumi_matlab_fft"
  c_matlab_fft :: Ptr (Complex Double)
               -> CSize
               -> Ptr (Complex Double)
               -> IO Bool

{-# NOINLINE fftMatLab #-}
-- | FFT by calling MatLab "fft" function
fftMatLab :: UV.Vector (Complex Double) -> Maybe (UV.Vector (Complex Double))
fftMatLab xs = unsafePerformIO $
  SV.unsafeWith (GV.convert xs) $ \xsPtr -> do
    ysPtr <- mallocForeignPtrArray n
    succeeded <- withForeignPtr ysPtr $ \outputPtr ->
      c_matlab_fft xsPtr (fromIntegral n) outputPtr
    pure $
      if succeeded
         then Just . GV.convert $ SV.unsafeFromForeignPtr0 ysPtr n
         else Nothing
  where
  n = UV.length xs

