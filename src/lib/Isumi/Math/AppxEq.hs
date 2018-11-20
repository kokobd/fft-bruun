{-# LANGUAGE FlexibleInstances #-}

module Isumi.Math.AppxEq
  ( AppxEq(..)
  , (~/=)
  ) where

import           Data.Complex
import qualified Data.Vector.Unboxed as UV

class AppxEq a where
  (~=) :: a -> a -> Bool

infix 4 ~=

infix 4 ~/=
(~/=) :: AppxEq a => a -> a -> Bool
x ~/= y = not $ x ~= y

instance AppxEq Int where
  x ~= y = x == y

instance AppxEq Double where
  x ~= y = (x - y) < 0.000001

instance AppxEq a => AppxEq (Complex a) where
  (r :+ v) ~= (r2 :+ v2) = (r ~= r2) && (v ~= v2)

instance AppxEq a => AppxEq (Maybe a) where
  Just x ~= Just y = x ~= y
  Nothing ~= Nothing = True
  _ ~= _ = False

instance AppxEq a => AppxEq [a] where
  xs ~= ys = and $ zipWith (~=) xs ys

instance (UV.Unbox a, AppxEq a) => AppxEq (UV.Vector a) where
  xs ~= ys = UV.toList xs ~= UV.toList ys

instance (AppxEq a, AppxEq b) => AppxEq (a, b) where
  (x1, y1) ~= (x2, y2) = x1 ~= x2 && y1 ~= y2

