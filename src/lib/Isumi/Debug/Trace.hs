{-# LANGUAGE ScopedTypeVariables #-}

module Isumi.Debug.Trace
  ( traceShowAs
  , traceShowMAs
  , traceShowIdAs
  , Proxy(..)
  ) where

import           Data.Proxy
import           Debug.Trace
import           Unsafe.Coerce

traceShowAs :: forall a b c. Show b => Proxy b -> a -> c -> c
traceShowAs _ x y =
  let b = unsafeCoerce x :: b
   in traceShow b y

traceShowMAs :: forall a b f. (Show b, Applicative f) => Proxy b -> a -> f ()
traceShowMAs _ x =
  let b = unsafeCoerce x :: b
   in traceShowM b

traceShowIdAs :: forall a b. Show b => Proxy b -> a -> a
traceShowIdAs _ x =
  let b = unsafeCoerce x :: b
   in trace (show b) x
