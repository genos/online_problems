{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SpaceAge
  (Planet(..)
  ,ageOn)
  where

newtype Seconds = S Double deriving (Eq, Ord, Num, Fractional, Real)

data Planet
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

ageOn :: Planet -> Seconds -> Seconds
ageOn p = (/ (period * earthYear))
 where
  earthYear  = 31557600
  period = case p of
    Earth   -> 1
    Mercury -> 0.2408467
    Venus   -> 0.61519726
    Mars    -> 1.8808158
    Jupiter -> 11.862615
    Saturn  -> 29.447498
    Uranus  -> 84.016846
    Neptune -> 164.79132
