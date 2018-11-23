{-# LANGUAGE OverloadedLists #-}

module Isumi.Math.FFT.PolynomialSpec
  ( spec
  )
where

import           Isumi.Hspec
import           Test.Hspec

import           Data.Complex
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
    it "returns -2+2i for 4x^3+3x^2+2x+1 `modPoly` x+i" $ do
      let p = PolyVecRep [4, 3, 2, 1] :: Polynomial (Complex Double)
          d = PolyExpCoefRep [(1, 1), (0, 0 :+ 1)]
      (p `modPoly` d) `shouldBeAppx` Just (PolyVecRep [(-2) :+ 2])
  describe "polyMultiply'" $
    it "returns (x^3 + x) for x and (x^2 + 1)" $ do
      let expected = [(3, 1), (1, 1)] :: [(Int, Double)]
      let actual = (1, 1) `polyMultiply'` [(2, 1), (0, 1)]
      expected `shouldBeAppx` actual
  describe "factorDivisor" $ do
    it "factors x^2 - 1 into (x - 1)(x + 1)" $
      factorDivisor (BruunDivisorMinus 2) `shouldBeAppx`
        Just (BruunDivisorMinus 1, BruunDivisorPlus 1 (0 :: Double))
    it "factors x + 1 into Nothing" $
      factorDivisor (BruunDivisorComplex (1 :: Complex Double)) `shouldBeAppx`
        Nothing
    it "factors x^4+1.5x^2+1 into (x^2+(sqrt 0.5)x+1)(x^2-(sqrt 0.5)x+1)" $ do
      let c = sqrt 0.5 :: Double
      factorDivisor (BruunDivisorPlus 4 1.5) `shouldBeAppx`
        Just (BruunDivisorPlus 2 c, BruunDivisorPlus 2 (-c))

