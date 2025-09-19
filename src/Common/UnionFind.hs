module Common.UnionFind (
 createUnionFind,
 find,
 union,
 UFSet,
) where

import qualified Data.Vector.Unboxed.Mutable as V

type UFSet = V.IOVector Int

createUnionFind :: Int -> IO UFSet
createUnionFind n = do
 vec <- V.new n
 mapM_ (\i -> V.write vec i i) [0 .. n - 1]
 return vec

find :: UFSet -> Int -> IO Int
find vec x = do
 parent <- V.read vec x
 if parent == x
  then return x
  else do
   root <- find vec parent
   V.write vec x root
   return root

union :: UFSet -> Int -> Int -> IO ()
union vec x y = do
 rootX <- find vec x
 rootY <- find vec y
 V.write vec rootX rootY