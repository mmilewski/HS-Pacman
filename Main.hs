module Main where

import Graphics.UI.SDL as SDL
import System.Exit
import System.Random
import System.CPUTime
import Data.List as List
import Data.Map as Map

type Object = Float
type Objects = [Object]
type SurfacesMap = Map String Surface
type TimeDelta = Float
type CpuTime = Integer

(window_width, window_height) = (800, 600)

img_smile = "smile"
img_pacman = "pacman"
img_all = [img_smile, img_pacman]

loadImages :: IO (SurfacesMap)
loadImages
    = do surfaces <- mapM (\name -> loadBMP $ name ++ ".bmp") img_all
         return $ Map.fromList $ zip img_all surfaces

displayObjects :: Surface -> Float -> [Surface] -> IO ()
displayObjects screen posx images
    = do blitSurface (List.head images)  Nothing screen (Just $ Rect (round posx) 50 0 0)
         return ()

displayObject :: Surface -> Vector -> Surface -> IO ()
displayObject screen (Vector x y) image
    = do blitSurface image Nothing screen (Just $ Rect (round x) (round y) 0 0 )
         return ()

display :: Player -> Objects -> SurfacesMap -> IO ()
display (Player pos) objects imagesMap =
  do let images = List.map snd $ Map.toList imagesMap
     screen <- getVideoSurface
     let format = surfaceGetPixelFormat screen in
       do red   <- mapRGB format 0xFF 0 0
          green <- mapRGB format 0 0xFF 0
          fillRect screen Nothing green
          fillRect screen (Just $ Rect 10 10 10 10) red
          mapM_ (\obj -> displayObjects screen obj images) objects
          displayObject screen pos (List.head images)
          SDL.flip screen

moveObjects :: Objects -> TimeDelta -> Objects
moveObjects objects dt
    = List.map updateObject objects
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

data GameData = GameData { objects :: Objects,
                           pacman :: Player
                         } deriving (Show)

handleEvent :: TimeDelta -> Event -> GameData -> IO(GameData)
handleEvent dt NoEvent gd = return gd
handleEvent dt SDL.Quit gd = exitWith ExitSuccess
handleEvent dt (KeyDown (Keysym _ _ 'q')) gd = exitWith ExitSuccess
handleEvent dt (KeyDown keysym) (GameData objs pacman) = handleKeyDown keysym where
    handleKeyDown (Keysym SDLK_RIGHT _ _) = return $ GameData objs (movePlayer pacman (Vector ( 40*dt) 0))
    handleKeyDown (Keysym SDLK_LEFT  _ _) = return $ GameData objs (movePlayer pacman (Vector (-40*dt) 0))
    handleKeyDown (Keysym SDLK_DOWN  _ _) = return $ GameData objs (movePlayer pacman (Vector 0 ( 40*dt)))
    handleKeyDown (Keysym SDLK_UP    _ _) = return $ GameData objs (movePlayer pacman (Vector 0 (-40*dt)))
handleEvent _ _ gd = return gd

loop :: CpuTime -> GameData -> SurfacesMap -> IO ()
loop startTime gameData images
    = do endTime <- getCPUTime
         let dt = (fromIntegral (endTime - startTime)) / (10^11)

         event <- pollEvent
         (GameData objects pacman) <- handleEvent dt event gameData
         let objects' = moveObjects objects dt

         display pacman objects' images

         loop endTime (GameData objects' pacman) images

main = withInit [InitVideo] $
    do screen <- setVideoMode window_width window_height 16 [SWSurface]
       setCaption "HS-Pacman" ""
       enableUnicode True
       images <- loadImages
       startTime <- getCPUTime
       loop startTime (GameData [50.0] (Player $ Vector 31 100)) images
