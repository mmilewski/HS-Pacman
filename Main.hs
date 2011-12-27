module Main where

import Prelude hiding (lookup)
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import System.Exit
import System.Random
import System.CPUTime
import Data.List as List hiding (lookup)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromJust)
import Data.Bits ((.&.))

type Object = Float
type Objects = [Object]
type SurfacesMap = Map String Surface
type TimeDelta = Float
type CpuTime = Integer

(window_width, window_height) = (800, 600)
boardSize = (16, 12)   -- width, height

img_placeholder = img_smile
img_smile = "smile"
img_pacman = "pacman"
img_board_empty = "board-empty"
img_board_bottom = "board-bottom"
img_board_right = "board-right"
img_board_left = "board-left"
img_board_top = "board-top"
img_all = [img_smile, img_pacman, img_board_empty, img_board_bottom, img_board_right, img_board_left, img_board_top]

loadImages :: IO (SurfacesMap)
loadImages
    = do surfaces <- mapM (\name -> Image.load $ concat ["gfx/", name, ".png"]) img_all
         return $ fromList $ zip img_all surfaces

displayObjects :: Surface -> Float -> [Surface] -> IO ()
displayObjects screen posx images
    = do blitSurface (head images)  Nothing screen (Just $ Rect (round posx) 50 0 0)
         return ()

displayObject :: Surface -> Vector -> Surface -> IO ()
displayObject screen (Vector x y) image
    = do blitSurface image Nothing screen (Just $ Rect (round x) (round y) 0 0 )
         return ()

displayBoardPiece :: Surface -> SurfacesMap -> (Int, Int) -> (Int, Int) -> IO ()
displayBoardPiece screen imagesMap (boardWidth, boardHeight) (i, piece)
    = do blit $ surfaceByPiece (piece .&. 1)
         blit $ surfaceByPiece (piece .&. 2)
         blit $ surfaceByPiece (piece .&. 4)
         blit $ surfaceByPiece (piece .&. 8)
         where blit surface = blitSurface surface Nothing screen (Just $ Rect (round $ tileW * col) (round $ row * tileH) 0 0) >> return ()
               surfaceByPiece p = fromJust $ lookup (nameByPiece p) imagesMap
               nameByPiece p = fromJust $ lookup p $ fromList [(0, img_board_empty), (1, img_board_top),
                                                               (2, img_board_right), (4, img_board_bottom), (8, img_board_left)]
               (tileW, tileH) = (50, 50)
               row = fromIntegral $ i `div` boardWidth
               col = fromIntegral $ i `mod` boardWidth

display :: GameData -> SurfacesMap -> IO ()
display (GameData objects (Player pos) board) imagesMap =
  do screen <- getVideoSurface
     green <- mapRGB (surfaceGetPixelFormat screen) 0 0xFF 0
     fillRect screen Nothing green
     mapM_ (\ip -> displayBoardPiece screen imagesMap boardSize ip) $ zip (List.iterate (+1) 0) board
     let placeholder = fromJust $ lookup img_placeholder imagesMap
     mapM_ (\obj -> displayObjects screen obj [placeholder]) objects
     displayObject screen pos (fromJust $ lookup img_pacman imagesMap)
     SDL.flip screen

moveObjects :: Objects -> TimeDelta -> Objects
moveObjects objects dt
    = map updateObject objects
      where updateObject obj = if obj > fromIntegral window_width then 0 else obj + 20.0 * dt


data Vector = Vector Float Float
vadd (Vector a b) (Vector c d) = Vector (a + c) (b + d)
vscale (Vector a b) factor = Vector (a * factor) (b * factor)
instance Show Vector where
    show (Vector a b) = "[" ++ (show a) ++ ", " ++ (show b) ++ "]"


data Player = Player { pos :: Vector
                     } deriving (Show)

movePlayer :: Player -> Vector -> Player
movePlayer (Player pos) pos' = Player (vadd pos pos')

type Board = [Int]
data GameData = GameData { objects :: Objects,
                           pacman :: Player,
                           board :: Board
                         } deriving (Show)

handleEvent :: TimeDelta -> Event -> GameData -> IO(GameData)
handleEvent dt SDL.NoEvent gd = return gd
handleEvent dt SDL.Quit gd = exitWith ExitSuccess
handleEvent dt (KeyDown (Keysym _ _ 'q')) gd = exitWith ExitSuccess
handleEvent dt (KeyDown keysym) (GameData objs pacman board) = handleKeyDown keysym where
    handleKeyDown (Keysym SDLK_RIGHT _ _) = return $ GameData objs (movePlayer pacman (Vector ( 40*dt) 0)) board
    handleKeyDown (Keysym SDLK_LEFT  _ _) = return $ GameData objs (movePlayer pacman (Vector (-40*dt) 0)) board
    handleKeyDown (Keysym SDLK_DOWN  _ _) = return $ GameData objs (movePlayer pacman (Vector 0 ( 40*dt))) board
    handleKeyDown (Keysym SDLK_UP    _ _) = return $ GameData objs (movePlayer pacman (Vector 0 (-40*dt))) board
handleEvent _ _ gd = return gd

loop :: CpuTime -> GameData -> SurfacesMap -> IO ()
loop startTime gameData images
    = do endTime <- getCPUTime
         let dt = (fromIntegral (endTime - startTime)) / (10^11)

         event <- pollEvent
         (GameData objects pacman board) <- handleEvent dt event gameData
         let objects' = moveObjects objects dt

         let gameData' = (GameData objects' pacman board)
         display gameData' images
         loop endTime gameData' images

main = withInit [InitVideo] $
    do screen <- setVideoMode window_width window_height 16 [SWSurface]
       setCaption "HS-Pacman" ""
       enableUnicode True
       images <- loadImages
       startTime <- getCPUTime
       loop startTime (GameData objects player board) images
       where objects = [50.0]
             player = (Player $ Vector 31 100)
             board = [ 9,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  3,
                       8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2,
                       8,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  1,  3,  0,  0,  2,
                       8,  0,  0, 15,  2,  0,  0,  0,  2,  0,  0,  0,  2,  0,  0,  2,
                       8,  0,  0,  0,  2,  0,  0,  0,  2,  0,  0,  0,  2,  0,  0,  2,
                       8,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  2,  0,  0,  2,
                       8,  0,  0,  2,  0,  2,  0,  0,  2,  0, 12,  4,  6,  4,  6,  2,
                       8,  0,  8,  2,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2,
                       8,  0, 12,  6,  4,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2,
                       8,  0,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  2,
                       8,  0,  0,  0,  2,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  2,
                      12,  4,  4,  4,  6,  4,  4,  4,  4,  6,  4,  4,  4,  4,  4,  6]
