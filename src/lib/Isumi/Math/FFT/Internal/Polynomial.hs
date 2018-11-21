{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Stability: alpha

Note that this is an __internal__ module. Use it at your own risk.
-}
module Isumi.Math.FFT.Internal.Polynomial
  (
  -- * Public APIs
    Polynomial(..)
  , polyToVecRep
  , polyFromVecRep
  , polyFromExpCoefRep
  , UnboxFloatingAppxEq
  , modPoly
  , BruunDivisor(..)
  , factorDivisor
  , divToPoly
  -- * Internal APIs
  , polyMultiply'
  ) where

import           Control.Monad.Loops
import           Control.Monad.ST
import           Data.Complex
import           Data.Foldable                 (for_)
import           Data.STRef
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Vector.Unboxed.Mutable   as MUV
import           Isumi.Math.AppxEq
import           Isumi.Math.FFT.Internal.Utils (findM, isSorted)

-- | A finite polynomial, represented in either vector or list of (exponent,
-- coefficient). Using these constructors implys no check for pre-conditions.
data Polynomial a = PolyVecRep (UV.Vector a)
                  | PolyExpCoefRep [(Int, a)]
                    deriving Show

-- | Construct a polynomial from a vector of coefficients.
--
-- Coefficients of higher degree entries are stored first.
polyFromVecRep :: (UV.Unbox a, Floating a, Eq a)
               => UV.Vector a -> Maybe (Polynomial a)
polyFromVecRep cs =
  if UV.length cs > 0 && cs UV.! 0 == 0
     then Nothing
     else Just (PolyVecRep cs)

-- | Construct a polynomial from a list of (exponent, coefficient) pairs
--
-- Higher degree entries are stored first. Unlike vector representation, this
-- representation doesn't need to store every entry, only those have a non-zero
-- coefficient.
polyFromExpCoefRep :: [(Int, a)] -> Maybe (Polynomial a)
polyFromExpCoefRep dcs = if isSorted False (fmap fst dcs)
                            then Just $ PolyExpCoefRep dcs
                            else Nothing

polyToVecRep :: Polynomial a -> Maybe (UV.Vector a)
polyToVecRep (PolyVecRep xs) = Just xs
polyToVecRep _               = Nothing

{-| Divisor in the Bruun's FFT algorithm.
'a' is some floating type, such as 'Double'

There are three types of divisors, each of them is represented by a constructor.
-}
data BruunDivisor a =
                      -- | \( x^n - 1 \)
                      BruunDivisorMinus Int
                      -- | \( x^n + cx^{\frac{n}{2}} + 1 \)
                    | BruunDivisorPlus Int a
                      -- | \( c_1 x + c_2 x \)
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

type UnboxFloatingAppxEq a = (UV.Unbox a, Floating a, AppxEq a)

{-|
Compute polynomial modulo. Only supports the situation where the first
polynomial is represented by vector,and the second polynomial is represented by
degree-coefficient
-}
modPoly :: UnboxFloatingAppxEq a
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
modPoly' :: forall a. UnboxFloatingAppxEq a
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

polySubtract' :: UnboxFloatingAppxEq a
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

-- | Factor a divisor into two divisors.
--
-- If current divisor is of degree 1, it can not be factored again,
-- then 'Nothing' will be returned.
factorDivisor :: RealFloat a
              => BruunDivisor a
              -> Maybe (BruunDivisor a, BruunDivisor a)
factorDivisor (BruunDivisorComplex _ _) = Nothing
factorDivisor (BruunDivisorMinus n) =
  if n == 1
     then Nothing
     else
       let n' = n `div` 2
        in Just (BruunDivisorMinus n', BruunDivisorPlus n' 0)
factorDivisor (BruunDivisorPlus n c) =
  if | n == 1 -> Nothing
     | n `rem` 4 == 0 -> Just $ factorDPToDPs n c
     | otherwise -> Just $ factorDPToDCs c

-- | Factor divisor in plus form into two divisors in plus form.
-- Doesn't check for pre-condition.
factorDPToDPs :: Floating a => Int -> a -> (BruunDivisor a, BruunDivisor a)
factorDPToDPs n c =
  let n' = n `div` 2
      c' = sqrt (2 - c)
   in (BruunDivisorPlus n' c', BruunDivisorPlus n' (-c'))

-- | Factor divisor in plus form into two divisors in complex form.
-- Doesn't check for pre-condition.
factorDPToDCs :: Floating a
              => a
              -> (BruunDivisor a, BruunDivisor a)
factorDPToDCs c = undefined

