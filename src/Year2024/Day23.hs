module Year2024.Day23 (solution1, solution2) where

import Data.List (intercalate, maximumBy, sort)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Set as S

type Vertex = String

type Graph = M.Map Vertex (S.Set Vertex)

processInput :: String -> Graph
processInput s = go M.empty nodes
 where
  ls = lines s
  nodes = map (\l -> (take 2 l, drop 3 l)) ls
  go g [] = g
  go g ((n1, n2) : ns) =
   go
    ( M.insertWith
       S.union
       n1
       (S.singleton n2)
       ( M.insertWith
          S.union
          n2
          (S.singleton n1)
          g
       )
    )
    ns

search :: Graph -> S.Set [Vertex]
search graph = go S.empty $ M.assocs graph
 where
  go s [] = s
  go s ((n, ns) : vs) =
   go
    ( S.union s $
       S.fromList
        [ r
        | n1 <- S.toList ns
        , n2 <- S.toList (graph M.! n1)
        , n `S.member` (graph M.! n2)
        , let r = sort [n, n1, n2]
        , r `S.notMember` s
        ]
    )
    vs

startWithT :: String -> Bool
startWithT ('t' : _) = True
startWithT _ = False

solution1 :: String -> String
solution1 =
 show
  . S.size
  . S.filter (any startWithT)
  . search
  . processInput

solution2 :: String -> String
solution2 s =
 intercalate ","
  . sort
  . maximumBy (comparing length)
  $ scan vtx []
 where
  graph = processInput s
  vtx = M.keys graph
  go v parts [] = map (v :) parts
  go v parts (n : ns) =
   go v (merge n parts) ns
  merge n [] = [[n]]
  merge n (p : ps)
   | all (\v -> n `S.member` (graph M.! v)) p =
     (n : p) : merge n ps
   | otherwise = p : merge n ps
  scan [] parts = parts
  scan (v : vs) parts =
   let parts' =
        go
         v
         []
         (S.toList $ graph M.! v)
    in scan
        vs
        (parts' ++ parts)
