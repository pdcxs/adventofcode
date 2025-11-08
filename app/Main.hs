{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import AdventOfCode (
  animations,
  solutions,
 )
import Data.List (intercalate)
import Data.List.Split (splitWhen)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Linear (V2 (V2))
import Raylib.Core
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util.Colors (black, white)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (readFile')
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Animation Configurations
fontSize :: Int
fontSize = 20

-- Max Window Width
maxWidth :: Int
maxWidth = 1000

-- Max Window Height
maxHeight :: Int
maxHeight = 1000

-- End of Animation Configurations

main :: IO ()
main = do
  args <- getArgs
  if head args == "generate"
    then generateNewFiles (drop 1 args)
    else
      if length args < 3
        || "-h" `elem` args
        || "--help" `elem` args
        then do
          printHelp
        else do
          let (year, day, question) = getTimeQuestion args
              test = isTest args
              inputFile = getFile (year, day, test)
          contents <- readFile inputFile
          if question < 3
            then
              let func = solutions M.! (year, day, question)
               in func contents
            else
              let func = animations M.! (year, day, question)
                  -- generate animation texts
                  anims = func contents
                  fps = getArgFPS args
                  title =
                    printf
                      ( "Animation for Day %d"
                          ++ " of Year %d, Question %d"
                      )
                      day
                      year
                      (question - 2)
               in animation anims fps title

getTimeQuestion ::
  [String] -> (Int, Int, Int)
getTimeQuestion args = (year, day, question)
 where
  qs = map read $ take 3 args
  year = head qs
  day = qs !! 1
  question = last qs

getFile :: (Int, Int, Bool) -> FilePath
getFile (year, day, test) =
  "inputs" </> show year </> file
 where
  file = prefix ++ formatNum day ++ ".txt"
  prefix = if test then "test" else "input"

getArgFPS :: [String] -> Int
getArgFPS args =
  if length args > 4 && args !! 3 == "test"
    || length args > 3 && args !! 3 /= "test"
    then fromMaybe 1 (readMaybe (last args))
    else 1

isTest :: [String] -> Bool
isTest args =
  length args > 3
    && args !! 3 == "test"

formatNum :: Int -> String
formatNum i = if i < 10 then '0' : s else s
 where
  s = show i

printHelp :: IO ()
printHelp = do
  putStrLn $
    "usage: program_name"
      ++ " year day question [test]"
  putStrLn $
    "For example: \"stack run --"
      ++ " 2024 1 1\" will run first question"
      ++ " of day 1, 2024 year."
  putStrLn $
    "For example: \"stack run --"
      ++ " 2024 1 2 test\" will run second"
      ++ " question of day 1, 2024 year with test input."
  putStrLn $
    "For example: \"stack run --"
      ++ " 2024 15 3 5\" will play animation of first"
      ++ " question of day 15, 2024 year"
      ++ " with normal input and FPS 5."
  putStrLn $
    "For example: \"stack run --"
      ++ " 2024 15 4 test\" will play animation of first"
      ++ " question of day 15, 2024 year with"
      ++ " test input and default FPS 1."
  putStrLn $
    "stack run generate year day to generate new files\n"
      ++ "For example: \"stack run generate 2022 3\" "
      ++ "will generate new files for 2022 day 3."
  putStrLn
    "For more information, please see README.md."

animation ::
  [String] -> Int -> String -> IO ()
animation ss fps title = do
  setTargetFPS 60
  let ls = lines (head ss)
      width = length (head ls)
      height = length ls
      w =
        fromIntegral (width * fontSize) * 0.54 ::
          Double
      h =
        fromIntegral (height * fontSize) * 1.13 ::
          Double
      w' = min maxWidth (ceiling w)
      h' = min maxHeight (ceiling h)
      c =
        Camera2D
          (V2 0.0 0.0)
          (V2 0.0 0.0)
          0.0
          1.0
      delay = 1.0 / fromIntegral fps
  win <- initWindow w' h' title
  font <- loadFont "assets/consola.ttf"
  loop win c font delay 0.0 (cycle ss)
 where
  loop winr cam ft frameTime currentTime as@(f : fs) = do
    shouldClose <- windowShouldClose
    if not shouldClose
      then do
        frt <- getFrameTime
        cam' <- updateCam cam
        beginDrawing
        beginMode2D cam'
        clearBackground white
        drawTextEx
          ft
          f
          (V2 0 0)
          (fromIntegral fontSize)
          0.0
          black
        endMode2D
        endDrawing
        let nt = frt + currentTime
            exceed = nt >= frameTime
            ans = if exceed then fs else as
            nt' = if exceed then 0.0 else nt
        loop winr cam' ft frameTime nt' ans
      else closeWindow (Just winr)
  loop _ _ _ _ _ [] = undefined

  updateCam :: Camera2D -> IO Camera2D
  updateCam cam = do
    isLeft <- isKeyDown KeyLeft
    isRight <- isKeyDown KeyRight
    isUp <- isKeyDown KeyUp
    isDown <- isKeyDown KeyDown
    isZoomIn <- isKeyDown KeyEqual
    isZoomOut <- isKeyDown KeyMinus
    let dir = getDir isLeft isRight isUp isDown
        zoom
          | isZoomIn = z + 0.01
          | isZoomOut = z - 0.01
          | otherwise = z
    return $
      cam
        { camera2D'offset = offset + dir
        , camera2D'zoom = zoom
        }
   where
    offset = camera2D'offset cam
    z = camera2D'zoom cam
    getDir l r u d =
      let v0 = V2 0 0
          v1 = if l then v0 + V2 1 0 else v0
          v2 = if r then v1 - V2 1 0 else v1
          v3 = if u then v2 + V2 0 1 else v2
       in if d then v3 - V2 0 1 else v3

generateNewFiles :: [String] -> IO ()
generateNewFiles [year, day] = do
  createDirectoryIfMissing
    True
    ("." </> "src" </> ("Year" ++ year))
  createDirectoryIfMissing
    True
    ("." </> "inputs" </> year)
  writeFile
    ( "."
        </> "inputs"
        </> year
        </> ("test" ++ day' ++ ".txt")
    )
    ""
  writeFile
    ( "."
        </> "inputs"
        </> year
        </> ("input" ++ day' ++ ".txt")
    )
    ""
  writeFile
    ( "."
        </> "src"
        </> ("Year" ++ year)
        </> ("Day" ++ day' ++ ".hs")
    )
    ( unlines
        [ concat
            [ "module Year"
            , year
            , ".Day"
            , day'
            , " (solution1, solution2)"
            , " where"
            ]
        , ""
        , "solution1 :: String -> IO ()"
        , "solution1 = undefined"
        , ""
        , "solution2 :: String -> IO ()"
        , "solution2 = undefined"
        ]
    )
  insertSolution
 where
  day' = formatNum (read day)
  insertSolution
    | day' == "01" = do
        writeFile
          ( "."
              </> "src"
              </> ("Year" ++ year)
              </> "Solutions.hs"
          )
          ( unlines
              [ concat
                  [ "module Year"
                  , year
                  , ".Solutions "
                  , "(solutions, animations) where"
                  ]
              , ""
              , "import qualified Data.Map as M"
              , "import qualified Year" ++ year ++ ".Day" ++ day'
              , ""
              , "solutions ::"
              , "  M.Map (Int, Int, Int) (String -> IO ())"
              , "solutions ="
              , "  M.fromList"
              , concat
                  [ "    [ (("
                  , year
                  , ", 1, 1), Year"
                  , year
                  , ".Day01.solution1)"
                  ]
              , concat
                  [ "    , (("
                  , year
                  , ", 1, 2), Year"
                  , year
                  , ".Day01.solution2)"
                  ]
              , "    ]"
              , ""
              , "animations ::"
              , "  M.Map"
              , "    (Int, Int, Int)"
              , "    (String -> [String])"
              , "animations = M.empty"
              ]
          )
        let advSrc = "." </> "src" </> "AdventOfCode.hs"
        advCts <- readFile' advSrc
        let [ moduleSec
              , importSec
              , solutionsSec
              , animationsSec
              ] = splitWhen null (lines advCts)
        let importSec' =
              importSec
                ++ ["import qualified Year" ++ year ++ ".Solutions"]
            solutionsSec' =
              let (_ : otherLines) = reverse solutionsSec
               in reverse
                    ( "    ]"
                        : ("    , Year" ++ year ++ ".Solutions.solutions")
                        : otherLines
                    )
            animationsSec' =
              let (_ : otherLines) = reverse animationsSec
               in reverse
                    ( "    ]"
                        : ("   ,  Year" ++ year ++ ".Solutions.animations")
                        : otherLines
                    )
        let newContent =
              unlines
                ( intercalate
                    [""]
                    [ moduleSec
                    , importSec'
                    , solutionsSec'
                    , animationsSec'
                    ]
                )
        writeFile advSrc newContent
    | otherwise = do
        let srcFile =
              "."
                </> "src"
                </> ("Year" ++ year)
                </> "Solutions.hs"
        content <- readFile' srcFile
        let sections = splitWhen null (lines content)
            [ moduleSec
              , importSec
              , solutionsSec
              , animationsSec
              ] = sections
            importSec' =
              importSec
                ++ [ "import qualified Year"
                       ++ year
                       ++ ".Day"
                       ++ day'
                   ]
            solutionsSec' =
              let (_ : otherLines) = reverse solutionsSec
               in reverse
                    ( "    ]"
                        : concat
                          [ "    , (("
                          , year
                          , ", "
                          , day
                          , ", "
                          , "2), Year"
                          , year
                          , ".Day"
                          , day'
                          , ".solution2)"
                          ]
                        : concat
                          [ "    , (("
                          , year
                          , ", "
                          , day
                          , ", "
                          , "1), Year"
                          , year
                          , ".Day"
                          , day'
                          , ".solution1)"
                          ]
                        : otherLines
                    )
        let newSolutionContent =
              unlines $
                intercalate
                  [""]
                  [ moduleSec
                  , importSec'
                  , solutionsSec'
                  , animationsSec
                  ]
        writeFile srcFile newSolutionContent
generateNewFiles _ = printHelp
