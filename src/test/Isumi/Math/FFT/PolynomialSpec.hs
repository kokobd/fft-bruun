{-# LANGUAGE OverloadedLists #-}

module Isumi.Math.FFT.PolynomialSpec
  ( spec
  )
where

import           Test.Hspec
import           Isumi.Hspec

import           Isumi.Math.FFT.Internal.Polynomial

spec :: Spec
spec = do
  describe "modPoly" $ do
    it "returns 5 for (x^3 - 2x^2 - 4) `modPoly` (x - 3)" $ do
      let p = PolyVecRep [1, -2, 0, -4] :: Polynomial Double
      let actual = p `modPoly` PolyExpCoefRep [(1, 1), (0, -3)]
      (actual >>= polyToVecRep) `shouldBeAppx` Just [5]
    it "returns 0 for 1 `modPoly` 1" $ do
      let actual = (PolyVecRep [1] :: Polynomial Double)
            `modPoly` PolyExpCoefRep [(0, 1)]
      (actual >>= polyToVecRep) `shouldBeAppx` Just [1]
  describe "polyMultiply'" $
    it "returns (x^3 + x) for x and (x^2 + 1)" $ do
      let expected = [(3, 1), (1, 1)] :: [(Int, Double)]
      let actual = (1, 1) `polyMultiply'` [(2, 1), (0, 1)]
      expected `shouldBeAppx` actual

