module Trie  (Trie(..), left, right, find, decode, toList) where
import Control.Exception (Exception)
import Bit
import Control.Exception (throw)
data TrieExceptionLeft = LeftOfException String deriving (Show)
instance Exception TrieExceptionLeft


data Trie a = Leaf a -- El leaf es un constructor del Trie a
              | Trie a :-: Trie a deriving (Eq, Show) -- El :-: es un constructor del Trie a


left::Trie a -> Trie a
left (Leaf _) = throw $ LeftOfException "Left of: Leaf"
left (x :-: _) = x

right::Trie a -> Trie a
right (Leaf _) = throw $ LeftOfException "Right of: Leaf"
right (_ :-: x) = x
  
find::Bits -> Trie a -> a
find [] (Leaf x) = x
find [] (_ :-: _) = throw $ LeftOfException "Find: Empty bits"
find (x: sx) trie
  | x == F = find sx (left trie)
  | otherwise = find sx (right trie)


decode::Bits -> Trie Char -> String
decode x trie = decodeSubFunc x "" trie trie


decodeSubFunc::Bits -> String -> Trie Char -> Trie Char-> String
decodeSubFunc [] str (Leaf a) _ = str ++ [a]
decodeSubFunc x string (Leaf a) completeTrie = decodeSubFunc x (string ++ [a]) completeTrie completeTrie
decodeSubFunc (x: sx) string (l :-: r) completeTrie
  |(x == F) = decodeSubFunc sx string l completeTrie
  | otherwise = decodeSubFunc sx string r completeTrie


toList::Trie a -> [(a, Bits)]
toList trie = preOrder trie [] []



preOrder:: Trie a -> Bits -> [(a, Bits)] -> [(a, Bits)]
preOrder (Leaf x) bits list = list ++ [(x, bits)]
preOrder (l :-: r) bits list = preOrder l (bits ++ [F]) list ++ preOrder r (bits ++ [T] ) list