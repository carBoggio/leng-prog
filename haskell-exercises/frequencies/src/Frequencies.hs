module Frequencies (Frequency, frequencyMap, frequencies, insert, insertionSort) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)

-- TP 2:
insert :: (Ord a) => a -> [a] -> [a]
insert i [] = [i]
insert i list1 =
  let greaterEqThan = foldr (\x acc -> if x >= i then x : acc else acc) [] list1
      lessThan = foldr (\x acc -> if x < i then x : acc else acc) [] list1
  in lessThan ++ [i] ++ greaterEqThan

insertionSort :: (Ord a) => [a] -> [a]
insertionSort y = foldr (\x acc -> insert x acc) [] y

-- Fin TP 2

-- counts the apparitions of an element in a list
countFrequency :: (Ord a) => [a] -> a -> Int
countFrequency inputList element = foldr (\listElement acc -> if listElement == element then acc + 1 else acc) 0 inputList

countFrequencyIfIsNotInList :: (Ord a) => [a] -> [a] -> Map a Int -> Map a Int
countFrequencyIfIsNotInList _ [] mapOf = mapOf
countFrequencyIfIsNotInList countedElements (x:sx) mapOf
  | x `elem` countedElements = countFrequencyIfIsNotInList countedElements sx mapOf
  | otherwise = countFrequencyIfIsNotInList (x:countedElements) sx (Map.insert x (countFrequency (x:sx) x) mapOf)

-- main function:
frequencyMap :: (Ord a) => [a] -> Map a Int
frequencyMap list = countFrequencyIfIsNotInList [] list Map.empty

-- Ej frequencies

type Frequency = (Int, Char)

-- main function:
frequencies :: String -> [Frequency]
frequencies string = insertionSort $ map swap $ Map.toList (frequencyMap string)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      content <- readFile fileName
      let freqs = frequencies content
      mapM_ print (reverse freqs)
    _ -> putStrLn "Error: Please provide a single file name as an argument."