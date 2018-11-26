{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Isumi.Math.FFT.BruunSpec
  ( spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Complex
import qualified Data.Vector.Unboxed  as UV
import           Isumi.Hspec          (shouldBeAppx)
import           Isumi.Math.AppxEq    ((~=))
import           Isumi.Math.FFT.Bruun

spec :: Spec
spec =
  describe "fftBruun/fftBruunC" $ do
    it "works for a simple sample" $
      fftBruun [1..4] `shouldBeAppx` Just
        [10 :+ 0, (-2) :+ 2, (-2) :+ 0, (-2) :+ (-2)]
    it "works for a simple complex sample" $
      fftBruunC [1 :+ 4, 2 :+ 3, 3 :+ 2, 4 :+ 1] `shouldBeAppx` Just
        [10 :+ 10, 0 :+ 4, (-2) :+ 2, (-4) :+ 0]
    it "works for input with trailing zeros" $
      fftBruun [1, 1, 0, 0] `shouldBeAppx` Just
        [2 :+ 0, 1 :+ (-1), 0, 1 :+ 1]
    it "gives same answer for all inputs as fftDirect" $ property $
      \(VectorOf2ExpSize xs) ->
        Just (fftDirect (realVToComplexV xs)) ~= fftBruun xs
    it "gives same answer for all complex inputs as fftDirect" $ property $
      \(VectorOf2ExpSize rs) (VectorOf2ExpSize cs) ->
        let xs = UV.zipWith (:+) rs cs
         in Just (fftDirect xs) ~= fftBruunC xs

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

