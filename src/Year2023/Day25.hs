module Year2023.Day25 (solution1, solution2) where

import Common.UnionFind (
 UFSet,
 createUnionFind,
 find,
 union,
 )
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import System.Random (
 RandomGen,
 mkStdGen,
 uniformShuffleList,
 )

type Vertex = Int
type Edge = (Vertex, Vertex)
type Group = Int
type VertexMap = M.Map Vertex Group

processInput :: String -> ([Edge], Int)
processInput input = (edges, M.size vertexMap)
 where
  ls = lines input
  getEdge ln = case splitOn ": " ln of
   [from, tos] -> [(from, to) | to <- words tos]
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
    ( \(m, i) v -> case M.lookup v m of
       Just _ -> (m, i)
       Nothing -> (M.insert v i m, i + 1)
    )
    (M.empty, 0)
    verts

isLoop :: UFSet -> Edge -> IO Bool
isLoop vs (a, b) = do
 r1 <- find vs a
 r2 <- find vs b
 return (r1 == r2)

mergeEdge :: UFSet -> [Edge] -> IO [Edge]
mergeEdge _ [] = error "No edges to merge"
mergeEdge vs ((from, to) : es) = do
 union vs from to
 loops <- traverse (isLoop vs) es
 return
  [ e
  | (e, isL) <- zip es loops
  , not isL
  ]

merge :: UFSet -> [Edge] -> IO Int
merge vs es
 | null es = return 0
 | otherwise =
   case es of
    [_, _, _] -> return 3
    [_, _] -> return 2
    [_] -> return 1
    [] -> return 0
    _ -> do
     es' <- mergeEdge vs es
     merge vs es'

search ::
 (RandomGen g) =>
 g -> Int -> Int -> [Edge] -> IO Int
search g cnt vc es = do
 let (es', g') = uniformShuffleList es g
 vs <- createUnionFind vc
 r <- merge vs es'
 roots <- traverse (find vs) [0 .. vc - 1]
 let gs =
      M.fromListWith
       (+)
       [(rt, 1) | rt <- roots]
     gc = M.size gs
 if r == 3 && gc == 2
  then
   return $ product gs
  else do
   putStrLn $
    "searching... " ++ show cnt
   search g' (cnt + 1) vc es

solution1 :: String -> IO ()
solution1 input =
 search (mkStdGen 1) 1 vs es
  >>= print
 where
  (es, vs) = processInput input

-- A Pure Functional Version
-- More elegant, but slower
processInput' :: String -> ([Edge], VertexMap)
processInput' input = (edges, vertexGroup)
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
    ( \(m, i) v -> case M.lookup v m of
       Just _ -> (m, i)
       Nothing -> (M.insert v i m, i + 1)
    )
    (M.empty, 0)
    verts
  vertexGroup =
   M.fromList
    [ (g, g)
    | v <- verts
    , let g = vertexMap M.! v
    ]

isLoop' :: VertexMap -> Edge -> Bool
isLoop' vm (a, b) = vm M.! a == vm M.! b

mergeEdge' ::
 VertexMap ->
 [Edge] ->
 ([Edge], VertexMap)
mergeEdge' _ [] = error "No edges to merge"
mergeEdge' vm ((from, to) : es) =
 (es', vm')
 where
  g1 = vm M.! from
  g2 = vm M.! to
  vm' =
   M.map
    ( \g ->
       if g == g2 then g1 else g
    )
    vm
  es' = filter (not . isLoop' vm') es

merge' ::
 VertexMap ->
 [Edge] ->
 (VertexMap, Int)
merge' vm es =
 case es of
  [_, _, _] -> (vm, 3)
  [_, _] -> (vm, 2)
  [_] -> (vm, 1)
  [] -> (vm, 0)
  _ ->
   let (es', vm') = mergeEdge' vm es
    in merge' vm' es'

search' ::
 (RandomGen g) =>
 g -> VertexMap -> Int -> [Edge] -> IO Int
search' g vm cnt es = do
 let (es', g') = uniformShuffleList es g
     (vm', r) = merge' vm es'
     groups :: M.Map Group Int
     groups =
      M.foldl'
       ( \gs gid ->
          M.insertWith (+) gid 1 gs
       )
       M.empty
       vm'
 if r == 3 && M.size groups == 2
  then
   return $ product groups
  else do
   putStrLn $
    "searching... " ++ show cnt
   search' g' vm (cnt + 1) es

solution2 :: String -> IO ()
solution2 input =
 search' (mkStdGen 2) vm 1 es
  >>= print
 where
  (es, vm) = processInput' input
