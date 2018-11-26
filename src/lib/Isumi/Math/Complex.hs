{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies      #-}

module Isumi.Math.Complex
  ( RealOrComplex(..)
  ) where

import           Data.Complex

class Floating a => RealOrComplex a where
  type ToComplex a
  type ToReal a
  toComplex :: a -> ToComplex a

instance RealOrComplex Double where
  type ToComplex Double = Complex Double
  type ToReal Double = Double
  toComplex x = x :+ 0

instance RealOrComplex (Complex Double) where
  type ToComplex (Complex Double) = Complex Double
  type ToReal (Complex Double) = Double
  toComplex = id
  
