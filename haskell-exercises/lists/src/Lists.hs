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
binaryAdd str1 str2 =
  let (n1, n2) = sameLength str1 str2
      (carry, resp) = foldl (\(acc, res) (x, y) ->
        let suma = (charToNumber x) + (charToNumber y) + acc
        in if suma == 0
           then (0, '0' : res)
           else if suma == 1
                then (0, '1' : res)
                else if suma == 2
                     then (1, '0' : res)
                     else (1, '1' : res)) (0, "") (zip n1 n2)
  in if carry == 1
     then '1' : resp
     else resp


sameLength :: String -> String -> (String, String)
sameLength str1 str2
  | (length str1) > (length str2) = sameLength str1 ("0" ++ str2)
  | (length str1) < (length str2) = sameLength ("0" ++ str1) str2
  | otherwise = (str1, str2)

