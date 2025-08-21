module Year2024.Day19 (solution1, solution2) where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.MemoTrie (memo)

-- a trie tree
-- https://www.geeksforgeeks.org/introduction-to-trie-data-structure-and-algorithm-tutorials/
-- bool indicates matched results
data Trie = Node !Bool (M.Map Char Trie)

-- two trie trees can be merged
instance Semigroup Trie where
 Node t1 m1 <> Node t2 m2 =
  Node
   (t1 || t2)
   (M.unionWith (<>) m1 m2)

-- empty tree or leaf
instance Monoid Trie where
 -- identity law: x <> mempty = x
 -- so bool field should be false
 mempty = Node False M.empty

-- construct a trie tree from a prefix
toTrie :: String -> Trie
toTrie = foldr cons (Node True M.empty)

-- prepend a char to a trie tree
cons :: Char -> Trie -> Trie
cons n tree = Node False (M.singleton n tree)

-- get trie tree contains all prefixes
getPrefixTree :: [String] -> Trie
getPrefixTree = foldMap toTrie

-- matchIndexes (getPrefixTree ["ab", "abc"]) 0 "abcd"
-- [2, 3]
matchIndexes :: Trie -> Int -> String -> [Int]
matchIndexes _ _ [] = []
matchIndexes (Node _ currentMap) n (c : cs) =
 case currentMap M.!? c of
  Just tree@(Node isMatch _) ->
   [n + 1 | isMatch]
    ++ matchIndexes tree (n + 1) cs
  _ -> []

countWays :: Trie -> String -> Int
countWays tree input = search 0
 where
  n = length input
  search i
   | i == n = 1
   | i < n =
     sum
      ( map
         -- same search start location
         -- have same result
         -- so we use memoize technology
         (memo search)
         (matchIndexes tree i (drop i input))
      )
   | otherwise = 0

processInput :: String -> (Trie, [String])
processInput s = (tree, inputs)
 where
  ls = lines s
  inputs = drop 2 ls
  patterns = splitOn ", " (head ls)
  tree = getPrefixTree patterns

solution1 :: String -> IO ()
solution1 s =
 print . length . filter (> 0) $
  map (countWays tree) inputs
 where
  (tree, inputs) = processInput s

solution2 :: String -> IO ()
solution2 s =
 print . sum $
  map (countWays tree) inputs
 where
  (tree, inputs) = processInput s
