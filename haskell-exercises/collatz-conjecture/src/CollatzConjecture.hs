module CollatzConjecture (collatz) where

collatzFun :: Integer -> Integer -> Maybe Integer
collatzFun n iteration
  | (n <= 0) = Nothing
  | (n == 1) = Just iteration
  | ((n `mod` 2) == 1) =  collatzFun (n * 3 + 1) (iteration + 1)
  | otherwise = collatzFun (n `div` 2) (iteration + 1)

collatz :: Integer -> Maybe Integer
collatz n =  collatzFun n 0