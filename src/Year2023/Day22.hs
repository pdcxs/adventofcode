{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2023.Day22 (solution1, solution2) where

import qualified Data.IntMap.Strict as M
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Vector as V

type Point = (Int, Int, Int)

-- two ends
type Brick = (Point, Point)

-- lookup above according to below bricks
type Above = M.IntMap (S.Set Int)

-- lookup below according to above bricks
type Below = M.IntMap (S.Set Int)
type Supports = (Above, Below)

processInput :: String -> [Brick]
processInput = map parseLine . lines
 where
  parseLine xs =
    ( (xmin, ymin, zmin)
    , (xmax, ymax, zmax)
    )
   where
    [l, r] = splitOn "~" xs
    [xmin, ymin, zmin] =
      map read $ splitOn "," l
    [xmax, ymax, zmax] =
      map read $ splitOn "," r

brickZRange :: Brick -> (Int, Int)
brickZRange
  ((_, _, zmin), (_, _, zmax)) =
    (zmin, zmax)

overlapsXY :: Brick -> Brick -> Bool
overlapsXY
  ((xmin1, ymin1, _), (xmax1, ymax1, _))
  ((xmin2, ymin2, _), (xmax2, ymax2, _)) =
    overlaps (xmin1, xmax1) (xmin2, xmax2)
      && overlaps
        (ymin1, ymax1)
        (ymin2, ymax2)
   where
    overlaps (amin, amax) (bmin, bmax) =
      amin <= bmax && bmin <= amax

simulateFall ::
  [Brick] ->
  (V.Vector Brick, Supports)
simulateFall bricks =
  foldl'
    fall
    (V.empty, (M.empty, M.empty))
    sortedBricks
 where
  sortedBricks =
    sortBy
      (comparing (fst . brickZRange))
      bricks
  fall (fallen, (above, below)) brick =
    ( fallen V.++ V.singleton newBrick
    , (above', below')
    )
   where
    (minZ, maxZ) = brickZRange brick
    potentialBelow =
      filter
        ( \i ->
            snd (brickZRange (fallen V.! i))
              < minZ
        )
        [0 .. V.length fallen - 1]
    actualBelow =
      filter
        ( \i ->
            overlapsXY (fallen V.! i) brick
        )
        potentialBelow
    maxBelowZ =
      foldr
        ( max
            . snd
            . brickZRange
            . (fallen V.!)
        )
        0
        actualBelow
    newSupports =
      S.fromList $
        filter
          ( \i ->
              snd
                ( brickZRange
                    (fallen V.! i)
                )
                == maxBelowZ
          )
          actualBelow
    newMinZ = maxBelowZ + 1
    height = maxZ - minZ
    newMaxZ = newMinZ + height
    ( (x1, y1, _)
      , (x2, y2, _)
      ) = brick
    newBrick =
      ( (x1, y1, newMinZ)
      , (x2, y2, newMaxZ)
      )
    newAbove = V.length fallen
    below' =
      M.insert
        newAbove
        newSupports
        below
    above' =
      updateAbove
        above
        newSupports
        newAbove
    updateAbove a supports index =
      foldl
        ( \acc s ->
            M.insertWith
              S.union
              s
              (S.singleton index)
              acc
        )
        a
        supports

isSafe :: Supports -> Int -> Bool
isSafe (above, below) index =
  let tops =
        M.findWithDefault
          S.empty
          index
          above
      bottoms =
        S.map
          ( \i ->
              M.findWithDefault
                S.empty
                i
                below
          )
          tops
   in null bottoms
        || all
          ( \s ->
              S.size s > 1
          )
          bottoms

solution1 :: String -> IO ()
solution1 input =
  print safeCount
 where
  bricks = processInput input
  (fallenBricks, supports) =
    simulateFall bricks
  safeCount =
    length $
      filter
        (isSafe supports)
        [0 .. V.length fallenBricks - 1]

solution2 :: String -> IO ()
solution2 input =
  print
    . sum
    . map
      ( search supports S.empty
          . S.singleton
      )
    $ indexes
 where
  bricks = processInput input
  (fallenBricks, supports) =
    simulateFall bricks
  indexes =
    [ 0
    .. V.length fallenBricks - 1
    ]

search ::
  Supports ->
  S.Set Int ->
  S.Set Int ->
  Int
search s@(above, below) fallens indexes
  | S.null indexes =
      -- don't count lowest brick
      S.size fallens - 1
  | otherwise =
      search s fallens' fallenTops
 where
  tops =
    S.foldr' S.union S.empty $
      S.map
        ( \i ->
            M.findWithDefault
              S.empty
              i
              above
        )
        indexes
  fallens' = S.union fallens indexes
  fallenTops =
    S.filter
      ( \t ->
          (below M.! t)
            `S.isSubsetOf` fallens'
      )
      tops

