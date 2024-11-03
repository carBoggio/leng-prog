module Count where
import qualified Data.PQueue.Prio.Min as PQ
import Data.PQueue.Prio.Min (MinPQueue)
import Trie (Trie(..))
countFrequency :: (Ord a) => [a] -> a -> Int
countFrequency inputList element = foldr (\listElement acc -> if listElement == element then acc + 1 else acc) 0 inputList

countFrequencyIfIsNotInList :: (Ord a) => [a] -> [a] -> MinPQueue Int (Trie a) -> MinPQueue Int (Trie a)
countFrequencyIfIsNotInList _ [] pq = pq
countFrequencyIfIsNotInList countedElements (x:sx) pq
  | x `elem` countedElements = countFrequencyIfIsNotInList countedElements sx pq
  | otherwise = countFrequencyIfIsNotInList (x:countedElements) sx (PQ.insert (countFrequency (x:sx) x) (Leaf x) pq)

-- main function:
count :: (Ord a) => [a] -> MinPQueue Int (Trie a)
count list = countFrequencyIfIsNotInList [] list PQ.empty