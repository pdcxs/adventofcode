module Year2024.Day09 (solution1, solution2) where

import Data.Char (ord)
import Data.List.Split (chunksOf)

type Loc = Int
type Id = Int
type Size = Int
type Span = (Loc, Size)
type File = (Span, Id)

processInput ::
  String -> ([File], [Span])
processInput s = (zip files [0 ..], frees)
 where
  go loc (size : ss) =
    getSpan loc size
      : go (loc + size) ss
  go _ [] = []
  getSpan loc size = (loc, size)
  spans =
    chunksOf 2 $
      go 0 $
        map (\c -> ord c - ord '0') s
  files = map head spans
  frees =
    concatMap
      (\x -> [last x | length x == 2])
      spans

-- process a file
-- get new files and new free spaces
update ::
  File -> [Span] -> ([File], [Span])
update _ [] =
  error "no enough free space"
update
  file@((fileLoc, fileSize), fileId)
  frees@((freeLoc, freeSize) : fs)
    | fileLoc <= freeLoc = ([file], frees) -- don't move this
    | fileSize <= freeSize =
        ( [((freeLoc, fileSize), fileId)]
        , [ ( freeLoc + fileSize
            , freeSize - fileSize
            )
          | freeSize > fileSize
          ]
            ++ fs
        )
    | otherwise =
        let (files', frees') = update file2 fs
         in (file1 : files', frees')
   where
    file1 = ((freeLoc, freeSize), fileId)
    file2 =
      ((fileLoc, fileSize - freeSize), fileId)

solution1 :: String -> IO ()
solution1 s =
  print $ getCheckSums sortedFiles
 where
  (files, frees) = processInput s
  go [] frs = ([], frs)
  go (f : fs) frs =
    let (f', frees') = go fs frs
        (newFile, newFree) = update f frees'
     in (newFile ++ f', newFree)
  (sortedFiles, _) = go files frees

getCheckSum :: File -> Int
getCheckSum ((loc, size), fileId) =
  sum $
    take size $
      map (* fileId) [loc ..]

getCheckSums :: [File] -> Int
getCheckSums = sum . map getCheckSum

solution2 :: String -> IO ()
solution2 s =
  print $ getCheckSums sortedFiles
 where
  (files, frees) = processInput s
  go [] frs = ([], frs)
  go (f : fs) frs =
    let (f', frees') = go fs frs
        (newFile, newFree) = update' f frees'
     in (newFile : f', newFree)
  (sortedFiles, _) = go files frees

-- even simpler than first question
-- process one file
-- get new file and new free spaces
update' ::
  File -> [Span] -> (File, [Span])
update' _ [] =
  error "no enough free space"
update'
  file@((fileLoc, fileSize), fileId)
  frees@(fr@(freeLoc, freeSize) : fs)
    | fileLoc <= freeLoc =
        (file, frees) -- don't move this
    | fileSize > freeSize =
        let (newFile, newFrees) = update' file fs
         in (newFile, fr : newFrees)
    | otherwise =
        ( ((freeLoc, fileSize), fileId)
        , newFree ++ fs
        )
   where
    newFree =
      [ ( freeLoc + fileSize
        , freeSize - fileSize
        )
      | freeSize > fileSize
      ]
