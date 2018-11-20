{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Isumi.Math.FFT.Internal.Polynomial where

import           Control.Monad.Loops
import           Control.Monad.ST
import           Data.Complex
import           Data.Foldable                 (for_)
import           Data.STRef
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Vector.Unboxed.Mutable   as MUV
import           Isumi.Math.AppxEq
import           Isumi.Math.FFT.Internal.Utils (findM)

data Polynomial a = PolyVecRep (UV.Vector a)
                  | PolyExpCoefRep [(Int, a)]
                    deriving Show

polyToVecRep :: Polynomial a -> Maybe (UV.Vector a)
polyToVecRep (PolyVecRep xs) = Just xs
polyToVecRep _               = Nothing

{-| Divisor in the Bruun's FFT algorithm.
  'a' is some floating type, such as Double
-}
data BruunDivisor a = BruunDivisorMinus Int -- degree
                    | BruunDivisorPlus Int a -- degree and coefficient
                    | BruunDivisorComplex (Complex a) (Complex a)
                      deriving Show

{-|
  Convert 'BruunDivisor' to 'Polynomial'
-}
divToPoly :: forall a . (UV.Unbox a, RealFloat a)
          => BruunDivisor a
          -> Either (Polynomial (Complex a)) (Polynomial a)
divToPoly = \case
  BruunDivisorMinus deg -> Right $
    PolyExpCoefRep [(deg, 1), (0, -1)]
  BruunDivisorPlus deg c -> Right $
    PolyExpCoefRep [(deg, 1), (deg `div` 2, c), (0, 1)]
  BruunDivisorComplex c1 c0 -> Left $
    PolyExpCoefRep [(1, c1), (0, c0)]

type UnboxFloatingEq a = (UV.Unbox a, Floating a, AppxEq a)

{-|
Compute polynomial modulo. Only supports the situation where the first
polynomial is represented by vector,and the second polynomial is represented by
degree-coefficient
-}
modPoly :: UnboxFloatingEq a
        => Polynomial a
        -> Polynomial a
        -> Maybe (Polynomial a)
modPoly _ (PolyExpCoefRep [])               = Nothing
modPoly (PolyVecRep cs) (PolyExpCoefRep ic) = Just . PolyVecRep $ modPoly' cs ic
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
  let dDeg = polyDegree . PolyExpCoefRep $ d
  let dHeadCoef :: a = snd . head $ d
  r <- UV.thaw n
  rDeg <- newSTRef (polyDegree . PolyVecRep $ n)
  rIsZero <- ((==0) <$> readSTRef rDeg) >>= newSTRef

  whileM_ ((&&) <$> (not <$> readSTRef rIsZero)
                <*> ((>=dDeg) <$> readSTRef rDeg))
          $ do
    rc <- readSTRef rDeg >>= readCoefAtDeg r
    let tc = rc / dHeadCoef
    tDeg <- (\rDeg' -> rDeg' - dDeg) <$> readSTRef rDeg
    polySubtract' (r, rDeg, rIsZero) (polyMultiply' (tDeg, tc) d)

  rDeg_ <- readSTRef rDeg
  pure $ MUV.drop (MUV.length r - 1 - rDeg_) r

polyDegree :: UV.Unbox a
           => Polynomial a
           -> Int
polyDegree (PolyVecRep v) =
  let len = UV.length v
   in if len == 0
         then 0
         else len - 1
polyDegree (PolyExpCoefRep ic) =
  case ic of
    []    -> 0
    (x:_) -> fst x

readCoefAtDeg :: UV.Unbox a => UV.MVector s a -> Int -> ST s a
readCoefAtDeg v d = MUV.read v (MUV.length v - 1 - d)

updateCoefAtDeg :: UV.Unbox a => UV.MVector s a -> Int -> a -> ST s ()
updateCoefAtDeg v d = MUV.write v (MUV.length v - 1 - d)

polyMultiply' :: Num a => (Int, a) -> [(Int, a)] -> [(Int, a)]
polyMultiply' (d, c) = fmap (\(d', c') -> (d + d', c * c'))

polySubtract' :: UnboxFloatingEq a
              => (UV.MVector s a, STRef s Int, STRef s Bool)
              -> [(Int, a)]
              -> ST s ()
polySubtract' (r, rDeg, rIsZero) dcs = do
  for_ dcs $ \(d, c) -> do
    c' <- (\x -> x - c) <$> readCoefAtDeg r d
    updateCoefAtDeg r d c'
  -- update 'rDeg' and 'rIsZero'
  -- search from rDeg to lower degrees, until a non-zero coefficient is found.
  rDeg_ <- readSTRef rDeg
  degM <- findM (fmap (~/= 0) . readCoefAtDeg r)
            [rDeg_, (rDeg_-1) .. 0]
  case degM of
    Nothing -> do
      writeSTRef rDeg 0
      writeSTRef rIsZero True
    Just d -> writeSTRef rDeg d

factorDivisor :: RealFloat a
              => BruunDivisor a
              -> Maybe (BruunDivisor a, BruunDivisor a)
factorDivisor = undefined

