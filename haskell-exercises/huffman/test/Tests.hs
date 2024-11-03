-- {-# LANGUAGE RecordWildCards #-}

--import Data.Foldable (for_)
import Test.Hspec    (describe, hspec, it, shouldBe)
import Trie
import Huffman
import Count
import qualified Data.PQueue.Prio.Min as PQ
import Data.PQueue.Prio.Min (MinPQueue)
import Data.List (maximumBy)

main :: IO ()
main = hspec $ do

  describe "huffman Trie" $ do
    it "Empty input" $ do
      huffmanTrie "" `shouldBe` Empty

    it "Single character input should return a Leaf with that character" $ do
      huffmanTrie "AAAA" `shouldBe` Leaf 'A'

    it "Two different characters" $ do
      huffmanTrie "ABAAB" `shouldBe` (Leaf 'B' :-: Leaf 'A')

    it "Two characters with different frequencies should create an imbalanced Trie" $ do
      huffmanTrie "AAB" `shouldBe` (Leaf 'B' :-: Leaf 'A')

    it "Multiple characters with specific structure" $ do
      let result = huffmanTrie "AAABCB"

      result `shouldBe` (Leaf 'A' :-: (Leaf 'C' :-: Leaf 'B'))

    it "Simple Text" $ do
     huffmanTrie "PAPAYA" `shouldBe` Leaf 'A' :-: (Leaf 'Y' :-: Leaf 'P')

    it "Longer" $ do
     huffmanTrie "zapallo con papa" `shouldBe`
        ((Leaf 'z' :-: Leaf ' ') :-: Leaf 'a')
           :-:
        (  (Leaf 'l' :-: Leaf 'o')
             :-:
           ((Leaf 'c' :-: Leaf 'n') :-: Leaf 'p'))

  describe "count" $ do
    it "Empty input" $ do
      PQ.null (count "" :: PQ.MinPQueue Int (Trie Char)) `shouldBe` True

    it "Simple Text" $ do
      let pq = count "PAPAYA" :: PQ.MinPQueue Int (Trie Char)
      PQ.size pq `shouldBe` 3
      PQ.findMin pq `shouldBe` (1, Leaf 'Y')

    it "All elements identical" $ do
      let pq = count "AAAAAA" :: PQ.MinPQueue Int (Trie Char)
      PQ.size pq `shouldBe` 1
      PQ.findMin pq `shouldBe` (6, Leaf 'A')

    it "Mixed characters with duplicates" $ do
      let pq = count "BANANA" :: PQ.MinPQueue Int (Trie Char)
      PQ.size pq `shouldBe` 3
      PQ.findMin pq `shouldBe` (1, Leaf 'B')

      let maxElement = maximumBy (\(p1, _) (p2, _) -> compare p1 p2) (PQ.toList pq)
      maxElement `shouldBe` (3, Leaf 'A')

    it "Special characters" $ do
      let pq = count "!@!!@@!" :: PQ.MinPQueue Int (Trie Char)
      PQ.size pq `shouldBe` 2
      PQ.findMin pq `shouldBe` (3, Leaf '@')


      let maxElement = maximumBy (\(p1, _) (p2, _) -> compare p1 p2) (PQ.toList pq)
      maxElement `shouldBe` (4, Leaf '!')



--  describe "encode" $ do
--    it "Empty input" $ do
--      doEncode "" `shouldBe` []
--
--    it "Simple Text" $ do
--      doEncode "PAPAYA" `shouldBe` [T,T,F,T,T,F,T,F,F]
--
--    it "Longer" $ do
--      doEncode "zapallo con papa" `shouldBe` [F,F,F,F,T,T,T,T,F,T,T,F,F,T,F,F,T,F,T,F,F,
--                                              T,T,T,F,F,T,F,T,T,T,F,T,F,F,T,T,T,T,F,T,T,T,T,F,T]
--
--  describe "decode"  $ do
--    it "Empty input"  $ do
--      doDecode "" [] `shouldBe` ""
--
--    it "Simple Text" $ do
--      doDecode "PAPAYA" [T,T,F,T,T,F,T,F,F] `shouldBe` "PAPAYA"
--
--    it "Longer" $ do
--      doDecode "zapallo con papa"
--        [F,F,F,F,T,T,T,T,F,T,T,F,F,T,F,F,T,F,T,F,F,
--         T,T,T,F,F,T,F,T,T,T,F,T,F,F,T,T,T,T,F,T,T,T,T,F,T] `shouldBe` "zapallo con papa"
--
doEncode :: String -> [Bit]
doEncode s = encode s (huffmanTrie s)

doDecode :: String -> [Bit] -> String
doDecode s bs = decode bs (huffmanTrie s) 