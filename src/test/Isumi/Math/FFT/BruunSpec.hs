{-# LANGUAGE OverloadedLists #-}

module Isumi.Math.FFT.BruunSpec
  ( spec
  ) where

import           Test.Hspec

import           Data.Complex
import           Isumi.Hspec          (shouldBeAppx)
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
