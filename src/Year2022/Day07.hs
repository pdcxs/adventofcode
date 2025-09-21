{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2022.Day07 (solution1, solution2) where

import Control.Monad.State
import qualified Data.Map.Strict as M

type Path = String
data File = File Path Int
 deriving (Show)

type Machine = M.Map Path [File]
type SizeRecord = M.Map Path Int

buildMachine :: Path -> Machine -> [String] -> Machine
buildMachine _ m [] = m
buildMachine cwd m (l : ls)
 | "$ cd /" == l = buildMachine "/" m ls
 | "$ cd .." == l = buildMachine (getParent cwd) m ls
 | "$ ls" == take 4 l = buildMachine cwd m ls
 | "$ cd " == take 5 l =
   let dir = drop 5 l
    in buildMachine (cwd ++ dir ++ "/") m ls
 | "dir " == take 4 l =
   let file = File (cwd ++ drop 4 l ++ "/") 0
       updatedFiles = file : M.findWithDefault [] cwd m
       updatedMachine = M.insert cwd updatedFiles m
    in buildMachine cwd updatedMachine ls
 | otherwise =
   let (sizeStr : fileName : _) = words l
       size = read sizeStr
       file = File (cwd ++ fileName) size
       updatedFiles = file : M.findWithDefault [] cwd m
       updatedMachine = M.insert cwd updatedFiles m
    in buildMachine cwd updatedMachine ls
 where
  getParent =
   reverse
    . dropWhile (/= '/')
    . tail
    . reverse

getSize :: Machine -> Path -> State SizeRecord Int
getSize m path = do
 sr <- get
 if M.member path sr
  then return $ sr M.! path
  else do
   let files = m M.! path
   size <- sum <$> mapM getFileSize files
   modify $ M.insert path size
   return size
 where
  getFileSize (File p s) =
   if last p == '/'
    then getSize m p
    else return s

getSizeRecord :: Machine -> SizeRecord
getSizeRecord m = execState (getSize m "/") M.empty

parseInput :: String -> SizeRecord
parseInput =
 getSizeRecord
  . buildMachine "/" (M.singleton "/" [])
  . lines

solution1 :: String -> IO ()
solution1 =
 print
  . sum
  . filter (<= 100000)
  . M.elems
  . parseInput

getDeleteSize :: SizeRecord -> Int
getDeleteSize sr =
 let totalUsed = sr M.! "/"
     target = totalUsed - 40000000
  in minimum $ filter (>= target) $ M.elems sr

solution2 :: String -> IO ()
solution2 = print . getDeleteSize . parseInput
