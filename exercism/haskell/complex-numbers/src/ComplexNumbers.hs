module ComplexNumbers (
 Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex
) where

import Prelude hiding (abs, div)

data Complex a = C { _re :: !a , _im :: !a} deriving (Eq, Show)

complex :: (a, a) -> Complex a
complex = uncurry C

conjugate :: Num a => Complex a -> Complex a
conjugate c = c { _im = negate . _im $ c }

abs :: Floating a => Complex a -> a
abs (C a b) = sqrt $ a * a + b * b

real :: Num a => Complex a -> a
real = _re

imaginary :: Num a => Complex a -> a
imaginary = _im

mul :: Num a => Complex a -> Complex a -> Complex a
mul (C a b) (C c d) = C (a * c - b * d) (b * c + a * d)

add :: Num a => Complex a -> Complex a -> Complex a
add (C a b) (C c d) = C (a + c) (b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (C a b) (C c d) = C (a - c) (b - d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (C a b) (C c d) = C ((a * c + b * d) / z) ((b * c - a * d) / z)
  where z = c * c + d * d
