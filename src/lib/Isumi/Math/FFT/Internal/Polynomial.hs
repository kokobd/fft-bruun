{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Isumi.Math.FFT.Internal.Polynomial
  ( Polynomial
  , polyFromVec
  , BruunDivisor(..)
  , divToPoly
  , modPoly
  , factorDivisor
  ) where

import           Control.Monad.Loops
import           Control.Monad.ST
import           Data.Complex
import           Data.Foldable                 (for_)
import           Data.STRef
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Vector.Unboxed.Mutable   as MUV
import           Isumi.Math.FFT.Internal.Utils (findM)

data Polynomial a = PolyVecRep (UV.Vector a)
                  | PolyCoefPowRep [(Int, a)]

polyFromVec :: UV.Vector a -> Polynomial a
polyFromVec = PolyVecRep

{-| Divisor in the Bruun's FFT algorithm.
  'a' is some floating type, such as Double
-}
data BruunDivisor a = BruunDivisorMinus Int -- degree
                    | BruunDivisorPlus Int a -- degree and coefficient
                    | BruunDivisorComplex (Complex a) (Complex a)
                      deriving Show

divToPoly :: forall a . (UV.Unbox a, RealFloat a)
          => BruunDivisor a
          -> Either (Polynomial (Complex a)) (Polynomial a)
divToPoly = \case
  BruunDivisorMinus deg -> Right $
    PolyCoefPowRep [(deg, 1), (0, -1)]
  BruunDivisorPlus deg c -> Right $
    PolyCoefPowRep [(deg, 1), (deg `div` 2, c), (0, 1)]
  BruunDivisorComplex c1 c0 -> Left $
    PolyCoefPowRep [(1, c1), (0, c0)]

type UnboxFloatingEq a = (UV.Unbox a, Floating a, Eq a)

modPoly :: UnboxFloatingEq a
        => Polynomial a
        -> Polynomial a
        -> Maybe (Polynomial a)
modPoly _ (PolyCoefPowRep [])               = Nothing
modPoly (PolyVecRep cs) (PolyCoefPowRep ic) = Just . PolyVecRep $ modPoly' cs ic
modPoly _ _                                 = Nothing

{-|
Basically follows the pseudo code from Wikipedia:
> function n / d:
>   require d ≠ 0
>   q ← 0
>   r ← n       # At each step n = d × q + r
>   while r ≠ 0 AND degree(r) ≥ degree(d):
>      t ← lead(r)/lead(d)     # Divide the leading terms
>      q ← q + t
>      r ← r − t * d
>   return (q, r)
-}
modPoly' :: forall a. UnboxFloatingEq a
         => UV.Vector a
         -> [(Int, a)]
         -> UV.Vector a
modPoly' n d = UV.create $ do
  let dDeg = polyDegree . PolyCoefPowRep $ d
  let dHeadCoef :: a = snd . head $ d
  r <- UV.thaw n
  rDeg <- newSTRef (polyDegree . PolyVecRep $ n)
  rIsZero <- ((==0) <$> readSTRef rDeg) >>= newSTRef
  whileM_ ((&&) <$> (not <$> readSTRef rIsZero)
                <*> ((>dDeg) <$> readSTRef rDeg))
          $ do
    rc <- (MUV.length r -) <$> readSTRef rDeg >>= MUV.read r
    let tc = rc / dHeadCoef
    tDeg <- (\rDeg' -> rDeg' - dDeg) <$> readSTRef rDeg
    polySubtract' (r, rDeg, rIsZero) (polyMultiply' (tDeg, tc) d)

  pure r

polyDegree :: UV.Unbox a
           => Polynomial a
           -> Int
polyDegree (PolyVecRep v) =
  let len = UV.length v
   in if len == 0
         then 0
         else len - 1
polyDegree (PolyCoefPowRep ic) =
  case ic of
    []    -> 0
    (x:_) -> fst x

polyMultiply' :: Num a => (Int, a) -> [(Int, a)] -> [(Int, a)]
polyMultiply' (d, c) = fmap (\(d', c') -> (d + d', c * c'))

polySubtract' :: UnboxFloatingEq a
              => (UV.MVector s a, STRef s Int, STRef s Bool)
              -> [(Int, a)]
              -> ST s ()
polySubtract' (r, rDeg, rIsZero) dcs = do
  for_ dcs $ \(d, c) -> do
    c' <- (\x -> x - c) <$> MUV.read r d
    MUV.write r d c'
  -- update 'rDeg' and 'rIsZero'
  -- search from rDeg to lower degrees, until a non-zero coefficient is found.
  rDeg_ <- readSTRef rDeg
  degM <- findM (fmap (/= 0) . MUV.read r) [rDeg_ .. 0]
  case degM of
    Nothing -> do
      writeSTRef rDeg 0
      writeSTRef rIsZero True
    Just d -> writeSTRef rDeg d

factorDivisor :: RealFloat a
              => BruunDivisor a
              -> Maybe (BruunDivisor a, BruunDivisor a)
factorDivisor = undefined
