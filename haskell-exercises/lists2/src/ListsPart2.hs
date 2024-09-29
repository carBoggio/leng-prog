module ListsPart2 (Bit(..), bitAt, charToBits, bits, queens) where

import Data.Char(ord)
import Data.Bits(testBit)

data Bit = F | T  deriving (Eq, Show, Enum, Read)
type Bits = [Bit]

bitAt :: Int -> Char -> Bit
bitAt n c = if testBit (ord c) (7-n) then T else F

charToBits :: Char -> Bits
charToBits c = [(bitAt x c) | x <-[0..7]]

bits::String -> Bits
bits s = foldl (\acc c -> acc ++ (charToBits c)) [] s

type Solution = [Int]
type Tuple = (Int, Int, Int, Int)

verifySafe::Tuple -> Bool
verifySafe (col1, col2, row1, row2) = (abs (col1 - col2)) /= (abs (row1 - row2))

anyTuple::[Int] -> Int -> Int -> Int -> Bool
anyTuple [] _ _ _ = False
anyTuple (x: xs) index len element
  | verifySafe (x, index, element, len) = True
  | otherwise = anyTuple xs (index + 1) len element

isSafe::Int -> Solution -> Bool
isSafe column list
  | column `elem` list = False
  | otherwise = (anyTuple list 1 (length list) column)

extendSolution :: Int -> Solution -> [Solution]
extendSolution n solution
  | length solution == n = [solution]  -- Si la solución tiene n reinas, es válida
  | otherwise = concatMap (\col -> extendSolution n (col : solution)) validColumns
  where
    validColumns = [col | col <- [1..n], isSafe col solution]

queens::Int -> [Solution]
queens n
  | n == 1 = [[1]]
  | n == 2 = []
  | otherwise  = extendSolution n []
