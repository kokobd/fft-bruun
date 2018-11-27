{-# LANGUAGE ScopedTypeVariables #-}

module Isumi.Math.FFT.SpecUtil where

import Test.QuickCheck
import Data.Complex
import qualified Data.Vector.Unboxed as UV

newtype VectorOf2ExpSize = VectorOf2ExpSize (UV.Vector Double)

instance Show VectorOf2ExpSize where
  show (VectorOf2ExpSize v) = show v

instance Arbitrary VectorOf2ExpSize where
  arbitrary = do
    e :: Int <- choose (0, 10)
    let n = 2^e
    xs <- sequenceA (replicate n arbitrary)
    pure . VectorOf2ExpSize . UV.fromList $ xs

realVToComplexV :: UV.Vector Double -> UV.Vector (Complex Double)
realVToComplexV = UV.map (\x -> x :+ 0)

