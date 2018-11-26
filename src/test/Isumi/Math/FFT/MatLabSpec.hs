{-# LANGUAGE OverloadedLists #-}

module Isumi.Math.FFT.MatLabSpec
  ( spec
  ) where

import           Test.Hspec

import           Data.Complex
import           Isumi.Hspec
import           Isumi.Math.FFT.MatLab

spec :: Spec
spec =
  describe "fftMatLab" $
    it "works for a simple input" $
      fftMatLab [1 :+ 5, 2 :+ 6, 3 :+ 7, 4 :+ 8] `shouldBeAppx`
        Just [10 :+ 26, (-4) :+ 0, (-2) :+ (-2), 0 :+ (-4)]
