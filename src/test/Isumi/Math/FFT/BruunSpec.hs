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
  describe "fftBruun" $
    it "works for a simple sample" $
      fftBruun [1..4] `shouldBeAppx` Just 
        [10 :+ 0, (-2) :+ 2, (-2) :+ 0, (-2) :+ (-2)]
        
