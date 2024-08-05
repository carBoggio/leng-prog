module Fraction (Fraction, add, sub, mul, divide, hcf) where

type Fraction = (Int, Int)

-- Implement the `add` Function

add :: Fraction -> Fraction -> Fraction
add (num1, denom1) (num2, denom2)
  | denom1 == denom2 = simpFunc (num2+num1, denom1)
  | otherwise = simpFunc (num1 * denom2 + num2 * denom1 , denom1 * denom2)
-- Implement the `sub` Function

sub :: Fraction -> Fraction -> Fraction
sub (num1, denom1) (num2, denom2)
  | denom1 == denom2 = simpFunc (num2-num1, denom1)
  | otherwise = simpFunc (num1 * denom2 - num2 * denom1 , denom1 * denom2)


-- Implement the `mul` Function

mul :: Fraction -> Fraction -> Fraction
mul (num1, denom1) (num2, denom2) = simpFunc (num1 * num2, denom1 * denom2)

-- Implement the `divide` Function

divide :: Fraction -> Fraction -> Fraction
divide (num1, denom1) (num2, denom2) = simpFunc (mul (num1, denom1) (denom2, num2))

-- Implement the `hcf` Function

hcf :: Int -> Int -> Int
hcf a b
  | (b == 0) = a
  | otherwise = hcf b (a `mod` b)

simpFunc:: Fraction -> Fraction
simpFunc (n1, n2) = (n1  `div` (hcf n1 n2), n2  `div` (hcf n1 n2))

  
