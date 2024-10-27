module Lists (firsts, member, union, intersection, difference,
              insert, insertionSort,
              binaryToDecimal, toDecimal, toDec, decimal,
              binaryAdd) where
  
import Data.Char(digitToInt)  

member:: Int -> [Int] -> Bool
member _ []      = False
member e (x:xs)  = e == x || member e xs


union:: [Int] -> [Int] -> [Int]
union [] ys     = ys
union (x:xs) ys 
  | member x ys = union xs ys
  | otherwise   = x : union xs ys

-- Remove Implementations, from, here on

intersection:: [Int] -> [Int] -> [Int]
-- debuelve los elementos que estan en las dos listas.
intersection list1 list2 = foldr (\x acc -> if (member x list2) then x:acc else acc) [] list1




difference:: [Int] -> [Int] -> [Int]
-- debuelve los elementos que estan en la primera pero no en la segunda
difference list y = foldr (\x acc -> if (member x y) then acc else x:acc) [] list


insert:: Int -> [Int] -> [Int]
insert i [] = [i]
insert i list1 =
  let graterEqThan  = foldr (\x acc -> if (x >= i) then (x: acc) else acc) [] list1
      lessThan = foldr (\x acc -> if (x < i) then (x: acc) else acc) [] list1
      in lessThan ++ [i] ++ graterEqThan



insertionSort :: [Int] -> [Int]
insertionSort y = foldr (\x acc -> insert x acc) [] y


binaryToDecimal :: [Int] -> Int
binaryToDecimal list = foldr (\(i, x) acc -> (x * (2 ^ i)) + acc) 0 (zip [0..] (reverse list))



toDecimal :: Int -> [Int] -> Int
toDecimal b list = foldr (\(i, x) acc -> (x * (b ^ i)) + acc) 0 (zip [0..] (reverse list))

    
toDec::Int -> String -> Int
toDec base chars = foldr (\(i, char) acc -> (charToNumber char) * (base ^ i) + acc) 0 (zip [0..] (reverse chars))

-- Same as `toDec` But use a list comprehension

decimal::Int -> String -> Int
decimal base chars = foldl (\acc (i, char) -> (charToNumber char) * (base ^ i) + acc) 0 (zip [0..] (reverse chars))

firsts::[a] -> [[a]]
firsts [] = []
firsts list = foldr(\n acc -> (take n list):acc ) [] [1..length list]

-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation


charToNumber :: Char -> Int
charToNumber c = digitToInt c

binaryAdd :: String -> String -> String
binaryAdd a b
    | null a && null b = "0"
binaryAdd xs ys = reverse (addBits (reverse xs) (reverse ys) 0)
  where
    addBits [] [] 0 = []
    addBits [] [] carry = if carry == 1 then "1" else []
    addBits (x:xs') [] carry = addBits (x:xs') ['0'] carry
    addBits [] (y:ys') carry = addBits ['0'] (y:ys') carry
    addBits (x:xs') (y:ys') carry =
      let sumBit = (bitValue x) + (bitValue y) + carry
          resultBit = if sumBit `mod` 2 == 0 then '0' else '1'
          newCarry = if sumBit > 1 then 1 else 0
      in resultBit : addBits xs' ys' newCarry

    bitValue '0' = 0
    bitValue '1' = 1
    bitValue _ = error "Invalid binary digit"
