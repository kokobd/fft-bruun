module Isumi.Hspec
  ( shouldBeWith
  , shouldBeAppx
  ) where

import           Isumi.Math.AppxEq
import           Test.Hspec

shouldBeWith :: Show a => (a -> a -> Bool) -> a -> a -> Expectation
shouldBeWith f x y = EqAny f x `shouldBe` EqAny f y

data EqAny a = EqAny (a -> a -> Bool) a

instance Eq (EqAny a) where
  EqAny f x == EqAny _ y = f x y

instance Show a => Show (EqAny a) where
  show (EqAny _ x) = show x

shouldBeAppx :: (Show a, AppxEq a) => a -> a -> Expectation
shouldBeAppx = shouldBeWith (~=)

