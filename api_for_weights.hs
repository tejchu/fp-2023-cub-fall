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

instance Add (Weight Kilograms) (Weight Kilograms) where
  add (Weight kg1) (Weight kg2) = Weight $ kg1 + kg2

instance Add (Weight Pounds) (Weight Pounds) where
  add (Weight lb1) (Weight lb2) = Weight $ lb1 + lb2

instance Add (Weight Poods) (Weight Poods) where
  add (Weight pood1) (Weight pood2) = Weight $ pood1 + pood2
