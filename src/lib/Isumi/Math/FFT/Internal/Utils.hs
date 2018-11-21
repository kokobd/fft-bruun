module Isumi.Math.FFT.Internal.Utils
  ( realToComplex
  , virtToComplex
  , vecFromValues
  , compI
  , findM
  , isSorted
  ) where

import           Data.Complex
import           Data.Foldable               (traverse_)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as MGV

realToComplex :: (Real a, Fractional b) => a -> Complex b
realToComplex x = realToFrac x :+ 0

virtToComplex :: (Real a, Fractional b) => a -> Complex b
virtToComplex y = 0 :+ realToFrac y

vecFromValues :: (Traversable t, GV.Vector v a)
              => t (Int, a) -> v a
vecFromValues xs = GV.create $ do
  v <- MGV.new ((1+) . maximum . fmap fst $ xs)
  traverse_ (uncurry (MGV.write v)) xs
  pure v

compI :: Fractional b => Complex b
compI = virtToComplex (1 :: Int)

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x:xs) = do
  b <- p x
  if b then pure $ Just x
       else findM p xs

-- |Determine whether a list is sorted strictly.
isSorted :: Ord a
         => Bool -- ^ ascending if True, descending if False
         -> [a]
         -> Bool
isSorted _ [] = True
isSorted _ [_] = True
isSorted asc (x:y:zs) =
  (if asc then x < y else x > y) && isSorted asc zs

