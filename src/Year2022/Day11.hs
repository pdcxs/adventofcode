{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2022.Day11 (solution1, solution2) where

import Data.Char (isDigit)
import qualified Data.IntMap.Strict as M
import Data.List (sortBy)
import Data.List.Split (splitOn, splitWhen)
import Data.Ord

type Item = Int
type Trans = Int -> Int
type Id = Int
data Monkey
  = Monkey Trans Int (Id, Id)
type Monkeys = M.IntMap Monkey
type Items = M.IntMap [Item]
type Count = M.IntMap Int

processInput ::
  String -> (Int, Count, Monkeys, Items)
processInput input = (d, cts, mks, itms)
 where
  inputs = splitWhen null $ lines input
  d = product (M.map (\(Monkey _ t _) -> t) mks)
  mks = M.fromList . map parse $ inputs
  itms = M.fromList . map getItems $ inputs
  cts = M.fromList (map (,0) (M.keys mks))
  getItems (idLine : itemsLine : _) =
    (getNum idLine, getNums itemsLine)
  getItems _ = undefined
  parse xs =
    let [ idLine
          , _itemsLine
          , transLine
          , testLine
          , trueLine
          , falseLine
          ] = xs
     in ( getNum idLine
        , Monkey
            (getTrans transLine)
            (getNum testLine)
            ( getNum trueLine
            , getNum falseLine
            )
        )
  getNum =
    read
      . takeWhile isDigit
      . dropWhile (not . isDigit)
  getNums =
    map read
      . splitOn ", "
      . dropWhile (not . isDigit)
  getTrans = getFunc . drop 4 . words
  getFunc ["*", "old"] = (^ (2 :: Int))
  getFunc ["+", n] = (+) (read n)
  getFunc ["*", n] = (*) (read n)
  getFunc _ = undefined

transform ::
  Int ->
  (Int -> Int) ->
  Monkeys ->
  Count ->
  Items ->
  (Count, Items)
transform divisor process mks cnts =
  go cnts M.empty . M.toList
 where
  go c r [] = (c, r)
  go c r ((itmId, itms) : itmss) =
    let Monkey tr d (t, f) = mks M.! itmId
        current = itms ++ M.findWithDefault [] itmId r
        itms' = map ((`mod` divisor) . process . tr) current
        c' =
          M.insertWith
            (+)
            itmId
            (fromIntegral $ length itms')
            c
        r' =
          foldl'
            ( \acc itm ->
                M.insertWith
                  (++)
                  (if itm `mod` d == 0 then t else f)
                  [itm]
                  acc
            )
            (M.insert itmId [] r)
            itms'
     in go c' r' itmss

repeatTrans ::
  Int ->
  (Int -> Int) ->
  Int ->
  (Count, Monkeys, Items) ->
  Count
repeatTrans _ _ 0 (c, _, _) = c
repeatTrans d f n (c, mks, itms) =
  let (c', itms') = transform d f mks c itms
   in repeatTrans d f (n - 1) (c', mks, itms')

getAnswer :: Count -> Int
getAnswer =
  product
    . take 2
    . sortBy (comparing Down)
    . M.elems

solution1 :: String -> IO ()
solution1 input =
  let (d, c, mks, itms) = processInput input
   in print
        . getAnswer
        $ repeatTrans d (`div` 3) 20 (c, mks, itms)

solution2 :: String -> IO ()
solution2 input =
  let (d, c, mks, itms) = processInput input
   in print
        . getAnswer
        $ repeatTrans d id 10000 (c, mks, itms)