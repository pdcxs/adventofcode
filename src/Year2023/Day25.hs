module Year2023.Day25 (solution1, solution2) where

import qualified Data.IntMap.Strict as IM
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import System.Random (
  RandomGen,
  mkStdGen,
  uniformShuffleList,
 )

type Vertex = Int
type Edge = (Vertex, Vertex)

-- GroupId -> Vertex Count in corresponding group
type GroupCount = IM.IntMap Int

processInput ::
  String -> ([Edge], GroupCount)
processInput input = (edges, vertexGroup)
 where
  ls = lines input
  getEdge ln = case splitOn ": " ln of
    [from, tos] ->
      [ (from, to) | to <- words tos
      ]
    _ -> []
  es = concatMap getEdge ls
  edges =
    [ (vertexMap M.! a, vertexMap M.! b)
    | (a, b) <- es
    ]
  getVertex ln =
    let vts = words ln
     in (init (head vts) : tail vts)
  verts = concatMap getVertex ls
  (vertexMap, _) =
    foldl'
      ( \(m, i) v ->
          if M.member v m
            then (m, i)
            else
              (M.insert v i m, i + 1)
      )
      (M.empty, 0)
      verts
  vertexGroup =
    IM.fromList
      [ (g, 1)
      | v <- verts
      , let g = vertexMap M.! v
      ]

mergeEdge ::
  GroupCount ->
  [Edge] ->
  ([Edge], GroupCount)
mergeEdge _ [] = error "No edges to merge"
mergeEdge gc ((from, to) : es) =
  (es', gc')
 where
  ct1 = gc IM.! from
  ct2 = gc IM.! to
  gc' =
    IM.insert
      from
      (ct1 + ct2)
      (IM.delete to gc)
  es' =
    filter (uncurry (/=)) $
      map
        ( \(a, b) ->
            ( if a == to then from else a
            , if b == to then from else b
            )
        )
        es

merge ::
  GroupCount ->
  [Edge] ->
  (GroupCount, Int)
merge gc es
  | IM.size gc == 2 = (gc, length es)
  | null es = (gc, 0)
  | otherwise =
      let (es', gc') = mergeEdge gc es
       in merge gc' es'

search ::
  (RandomGen g) =>
  g ->
  GroupCount ->
  Int ->
  [Edge] ->
  IO Int
search g gc cnt es = do
  let (es', g') = uniformShuffleList es g
      (gc', r) = merge gc es'
  if r == 3
    then
      return $ product gc'
    else do
      putStrLn $
        "searching... " ++ show cnt
      search g' gc (cnt + 1) es

solution1 :: String -> IO ()
solution1 input =
  search (mkStdGen 2) gc 1 es
    >>= print
 where
  (es, gc) = processInput input

solution2 :: String -> IO ()
solution2 _ = print "We made it!"

