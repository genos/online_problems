module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits (testBit)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Bounded, Enum, Eq)

allergies :: Int -> [Allergen]
allergies score = filter (`isAllergicTo`score) [minBound .. maxBound]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo = flip testBit . fromEnum
