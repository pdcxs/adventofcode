module Year2023.Day08 (solution1, solution2) where

import Data.Char (isAlphaNum)
import Data.List.Split (splitWhen)
import qualified Data.Map.Strict as M

type Node = String
type Map = M.Map Node (Node, Node)

safeHead :: [a] -> a
safeHead [] = error "empty list"
safeHead (x : _) = x

processInput :: String -> (String, Map)
processInput s = (insts, m)
 where
  ls = lines s
  insts = safeHead ls
  m = M.fromList $ map extract $ drop 2 ls
  extract str =
   let nds =
        filter (not . null) $
         splitWhen (not . isAlphaNum) str
    in (safeHead nds, (nds !! 1, last nds))

getStep :: Map -> Node -> String -> Int
getStep m node (i : insts)
 | node == "ZZZ" = 0
 | i == 'L' = 1 + getStep m l insts
 | i == 'R' = 1 + getStep m r insts
 | otherwise = undefined
 where
  (l, r) = m M.! node
getStep _ _ [] = undefined

solution1 :: String -> IO ()
solution1 s = print $ getStep m "AAA" (cycle insts)
 where
  (insts, m) = processInput s

getRound :: Map -> String -> Node -> Int
getRound m insts node
 | last node == 'Z' = 0
 | otherwise = 1 + getRound m insts node'
 where
  node' = step node insts
  step n [] = n
  step n (i : is) =
   let (l, r) = m M.! n
    in if i == 'L'
        then step l is
        else step r is

solution2 :: String -> IO ()
solution2 s = print (length insts * minRnd)
 where
  (insts, m) = processInput s
  starts =
   [ node
   | node <- M.keys m
   , last node == 'A'
   ]
  rnds = map (getRound m insts) starts
  minRnd = foldr1 lcm rnds
