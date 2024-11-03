module Huffman  (huffmanTrie, encode, decode, Bit(..)) where
import qualified Data.PQueue.Prio.Min as PQ
import Data.PQueue.Prio.Min()
import Count (count)
import Trie (Trie(..))
data Bit = F | T deriving (Eq, Show)
type Bits = [Bit]  




recursiveDoTheTrie :: PQ.MinPQueue Int (Trie a) -> Trie a
recursiveDoTheTrie pq
  | PQ.size pq == 1 = snd $ PQ.findMin pq
  | otherwise =
      let ((count1, trie1), pq1) = PQ.deleteFindMin pq
          ((count2, trie2), pq2) = PQ.deleteFindMin pq1
          (left, right) = if count1 == count2 then (trie2, trie1) else (trie1, trie2)
      in recursiveDoTheTrie (PQ.insert (count1 + count2) (left :-: right) pq2)


huffmanTrie::String -> Trie Char
huffmanTrie [] = Empty
huffmanTrie input = let pq = count input in recursiveDoTheTrie pq




encode :: String -> Trie Char -> Bits
encode input code = error "Implement it"
  
decode::Bits -> Trie Char -> String
decode bits trie = error "Implement it"