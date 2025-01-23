module Main (main) where

import AdventOfCode (animations, solutions)
import Control.Monad (unless)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Foreign.C.Types (CInt)
import Linear (V2 (..))
import qualified SDL
import qualified SDL.Font
import System.Environment (getArgs)
import System.FilePath ((</>))

type Camera = V2 CInt

main :: IO ()
main = do
  args <- getArgs
  if length args < 3
    then do
      putStrLn $
        "usage: program_name"
          ++ " year day question [test]"
      putStrLn $
        "For example: \"stack run --"
          ++ " 2024 1 1\" will run first question"
          ++ " of day1, 2024 year."
      putStrLn $
        "For example: \"stack run --"
          ++ " 2024 1 2 test\" will run second"
          ++ " question of day1, 2024 year with test input."
    else do
      let qs = map read $ take 3 args
          year = head qs
          day = qs !! 1
          question = last qs
          file =
            if length args > 3 && args !! 3 == "test"
              then "test" ++ show day ++ ".txt"
              else "input" ++ show day ++ ".txt"
          filePath = "inputs" </> show year </> file
          delay =
            if length args > 4 && args !! 3 == "test"
              || length args > 3 && args !! 3 /= "test"
              then
                read (last args) :: Double
              else 1.0 :: Double
      contents <- readFile filePath
      if question < 3
        then
          let func = solutions M.! (year, day, question)
           in putStrLn $ func contents
        else do
          let func = animations M.! (year, day, question)
              anims = func contents
          SDL.initializeAll
          SDL.Font.initialize
          font <-
            SDL.Font.load
              "./assets/consola.ttf"
              18
          (cw, ch) <-
            SDL.Font.size
              font
              (T.pack "W")
          let (width, height) =
                calculateWindowSize cw ch anims
          window <-
            SDL.createWindow (T.pack "AoC Animation") $
              SDL.defaultWindow
                { SDL.windowInitialSize =
                    V2
                      width
                      height
                }
          renderer <-
            SDL.createRenderer
              window
              (-1)
              SDL.defaultRenderer

          let initalCamera = V2 0 0
          animate
            (realToFrac delay)
            renderer
            font
            anims
            initalCamera
          SDL.destroyRenderer renderer
          SDL.destroyWindow window

calculateWindowSize :: Int -> Int -> [String] -> (CInt, CInt)
calculateWindowSize charWidth lineHeight strs =
  let ls = lines $ head strs
      maxLineLength = (maximum . map length) ls
      maxLineCount = length ls
      width =
        fromIntegral
          (maxLineLength * charWidth)
          + 20
      height =
        fromIntegral
          (maxLineCount * lineHeight)
          + 20
   in (min 1000 width, min 1000 height)

animate ::
  NominalDiffTime ->
  SDL.Renderer ->
  SDL.Font.Font ->
  [String] ->
  Camera ->
  IO ()
animate delay renderer font strs camera = do
  startTime <- getCurrentTime
  let loop ss cam lastUpdateTime = do
        currentTime <- getCurrentTime
        events <- SDL.pollEvents
        let quit = any isQuitEvent events
            newCam = updateCamera cam events
            timeDiff = diffUTCTime currentTime lastUpdateTime
            (newSs, newLastUpdateTime) =
              if timeDiff >= delay
                then (tail ss, currentTime)
                else (ss, lastUpdateTime)
        unless quit $ do
          renderFrame renderer font (head ss) newCam
          SDL.present renderer
          loop newSs newCam newLastUpdateTime
  loop (cycle strs) camera startTime

updateCamera :: Camera -> [SDL.Event] -> Camera
updateCamera (V2 x y) = foldl handleEvent (V2 x y)
 where
  handleEvent cam event =
    case SDL.eventPayload event of
      SDL.KeyboardEvent
        ( SDL.KeyboardEventData
            _
            SDL.Pressed
            _
            (SDL.Keysym _ keycode _)
          ) ->
          case keycode of
            SDL.KeycodeUp -> cam + V2 0 (-10)
            SDL.KeycodeDown -> cam + V2 0 10
            SDL.KeycodeLeft -> cam + V2 (-10) 0
            SDL.KeycodeRight -> cam + V2 10 0
            _ -> cam
      _ -> cam

renderFrame ::
  SDL.Renderer ->
  SDL.Font.Font ->
  String ->
  Camera ->
  IO ()
renderFrame renderer font s camera = do
  SDL.rendererDrawColor
    renderer
    SDL.$= SDL.V4 255 255 255 255
  SDL.clear renderer
  SDL.rendererDrawColor
    renderer
    SDL.$= SDL.V4 0 0 0 255
  let linesOfText = lines s
  mapM_
    (renderLine renderer font camera)
    (zip [0 ..] linesOfText)
  SDL.present renderer

renderLine ::
  SDL.Renderer ->
  SDL.Font.Font ->
  Camera ->
  (Int, String) ->
  IO ()
renderLine
  renderer
  font
  (V2 camX camY)
  (lineNumber, lineText) = do
    surface <-
      SDL.Font.blended
        font
        (SDL.V4 0 0 0 255)
        (T.pack lineText)
    texture <-
      SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface
    SDL.TextureInfo
      { SDL.textureWidth = w
      , SDL.textureHeight = h
      } <-
      SDL.queryTexture texture
    let x = 10 + camX
        y =
          fromIntegral $
            10
              + lineNumber * fromIntegral h
              + fromIntegral camY
    SDL.copy
      renderer
      texture
      Nothing
      ( Just $
          SDL.Rectangle
            (SDL.P $ SDL.V2 x y)
            (SDL.V2 w h)
      )
    SDL.destroyTexture texture

isQuitEvent :: SDL.Event -> Bool
isQuitEvent event =
  case SDL.eventPayload event of
    SDL.WindowClosedEvent _ -> True
    SDL.KeyboardEvent
      ( SDL.KeyboardEventData
          _
          _
          _
          (SDL.Keysym _ SDL.KeycodeEscape _)
        ) -> True
    _ -> False
