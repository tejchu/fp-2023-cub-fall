{-# LANGUAGE DataKinds, KindSignatures, GeneralizedNewtypeDeriving, TypeApplications, ScopedTypeVariables #-}

module WeightUnits where

data Units = Kilograms | Pounds | Poods

newtype Weight (unit :: Units) = Weight Double
  deriving (Num, Fractional, Show)

class Convert a b where
  convert :: a -> b

instance Convert (Weight Kilograms) (Weight Pounds) where
  convert (Weight kg) = Weight $ kg * 2.20462

instance Convert (Weight Pounds) (Weight Kilograms) where
  convert (Weight lb) = Weight $ lb * 0.453592

instance Convert a a where
  convert = id

class Add a b where
  add :: a -> b -> a

instance Add (Weight unit) (Weight unit) where
  add (Weight w1) (Weight w2) = Weight $ w1 + w2
