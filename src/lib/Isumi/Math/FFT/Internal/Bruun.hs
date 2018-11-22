{-# LANGUAGE TupleSections #-}

module Isumi.Math.FFT.Internal.Bruun
  ( fftDirect
  , cruN
  , fftBruun
  )
where

import           Data.Bits                          (shiftL)
import           Data.Complex
import           Data.List                          (concatMap, sortOn)
import           Data.Maybe
import qualified Data.Vector.Unboxed                as UV
import           Math.NumberTheory.Logarithms       (integerLog2)

import           Isumi.Math.FFT.Internal.Polynomial
import           Isumi.Math.FFT.Internal.Utils

fftDirect :: UV.Vector Double -> UV.Vector (Complex Double)
fftDirect xs = UV.generate len genEntry
 where
  len = UV.length xs
  genEntry k = UV.sum . flip UV.imap xs $ \n xn ->
    realToComplex xn * exp (coef * fromIntegral n * realToComplex k)
  coef = (-2) * pi * compI / fromIntegral len

{-|
  Computes the complex roots of unity
-}
cruN :: Int -> UV.Vector (Complex Double)
cruN n = UV.generate n $ \k ->
  exp (realToComplex (2 * k) * pi * compI) / fromIntegral n

fftBruun :: UV.Vector Double -> Maybe (UV.Vector (Complex Double))
fftBruun xs =
  if (not . isPowerOf2) n
     then Nothing
     else let r0 = RealPolynomial . fromJust . polyFromVecRep . UV.reverse $ xs
              d0 = BruunDivisorMinus n
           in Just $ fftBruun' (cruN n) (d0, r0)
  where
  n = UV.length xs

fftBruun' :: UV.Vector (Complex Double) -- ^cru array
          -> DivisorRemainder -- ^initial divisor and remainder
          -> UV.Vector (Complex Double)
fftBruun' cruValues dr = UV.replicate n 0 UV.// indexToFFTVal
  where
  n = UV.length cruValues
  cruToFFT :: [(Complex Double, Complex Double)]
  cruToFFT = fmap (\(d, r) -> (finalDivisorToCRU d, finalRemainderToValue r))
           . factorDRAll $ dr
  cruToIndex :: [(Complex Double, Int)]
  cruToIndex = zip (UV.toList cruValues) [0..]

  fftVals = fmap snd . sortOnFirstVirt $ cruToFFT
  indicies = fmap snd . sortOnFirstVirt $ cruToIndex
  indexToFFTVal :: [(Int, Complex Double)]
  indexToFFTVal = zip indicies fftVals

sortOnFirstVirt :: [(Complex Double, b)] -> [(Complex Double, b)]
sortOnFirstVirt = sortOn (imagPart . fst)

finalDivisorToCRU :: BruunDivisor Double -> Complex Double
finalDivisorToCRU (BruunDivisorComplex c) = c
finalDivisorToCRU _                       =
  error "Final stage divisors MUST be in complex form"

finalRemainderToValue :: RealOrComplexPolynomial Double -> Complex Double
finalRemainderToValue (ComplexPolynomial p) =
  case p of
    PolyVecRep values ->
      if UV.length values == 1
         then values UV.! 0
         else error $ "Final stage remainder MUST have a length of 1. Actual: "
                      ++ show (UV.length values)
    PolyExpCoefRep _ ->
      error "Final stage remainder MUST have exp-coef representation"
finalRemainderToValue (RealPolynomial p) =
  finalRemainderToValue . ComplexPolynomial . mapPoly realToComplex $ p

-- | Recursively divisor-remainder pairs until they can not be factored
factorDRAll :: DivisorRemainder -> [DivisorRemainder]
factorDRAll dr = go [dr]
  where
  go :: [DivisorRemainder] -> [DivisorRemainder]
  go xs = let ys = concatMap (extractListFromMaybe . factorDR) xs
           in if length ys /= 2 * length xs then xs else go ys

type DivisorRemainder = (BruunDivisor Double, RealOrComplexPolynomial Double)

-- | Factor a divisor-remainder pair into TWO sub divisor remainder pairs.
factorDR :: DivisorRemainder -> Maybe [DivisorRemainder]
factorDR (d, r) =
  if divDegree d == 1
     then Nothing
     else fmap (fromJust . flip updateDR r) . pairToList <$> factorDivisor d

updateDR :: BruunDivisor Double -- ^new divisor
         -> RealOrComplexPolynomial Double -- ^old remainder
         -> Maybe DivisorRemainder -- ^(new divisor, new remainder)
updateDR d r = (d,) <$> r `modPolyG` divToPoly d

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

isPowerOf2 :: Int -> Bool
isPowerOf2 x = (1 `shiftL` integerLog2 (fromIntegral x)) == x

extractListFromMaybe :: Maybe [a] -> [a]
extractListFromMaybe Nothing   = []
extractListFromMaybe (Just xs) = xs

