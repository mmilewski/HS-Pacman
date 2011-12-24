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

(window_width, window_height) = (800, 600)

img_smile = "smile"
img_pacman = "pacman"

loadImages :: IO (SurfacesMap)
loadImages = let ext = ".bmp"
                 loadImg name = loadBMP $ name ++ ext
             in do smile <- loadImg $ img_smile
                   pacman <- loadImg $ img_pacman
                   return $ Map.fromList [
                                           (img_pacman, pacman),
                                           (img_smile, smile)
                                         ]

displayObjects :: Surface -> Float -> [Surface] -> IO ()
displayObjects screen posx images
    = do blitSurface (List.head images)  Nothing screen (Just $ Rect (round posx) 50 0 0 )
         return ()

display :: Objects -> SurfacesMap -> IO ()
display objects imagesMap =
  do let images = List.map snd $ Map.toList imagesMap
     screen <- getVideoSurface
     let format = surfaceGetPixelFormat screen in
       do red   <- mapRGB format 0xFF 0 0
          green <- mapRGB format 0 0xFF 0
          fillRect screen Nothing green
          fillRect screen (Just $ Rect 10 10 10 10) red
          mapM_ (\obj -> displayObjects screen obj images) objects
          SDL.flip screen

moveObjects :: Objects -> Float -> Objects
moveObjects objects dt
    = List.map updateObject objects
      where updateObject obj = if obj > fromIntegral window_width then 0 else obj + 20.0 * dt

data GameData = GameData { objects :: Objects,
                           pacman :: Object
                         } deriving (Show)

handleEvent dt NoEvent gd = return gd
handleEvent dt (KeyDown (Keysym SDLK_RIGHT _ _)) (GameData objs pacman) = return $ GameData objs (pacman + 40*dt)
handleEvent dt (KeyDown (Keysym SDLK_LEFT  _ _)) (GameData objs pacman) = return $ GameData objs (pacman - 40*dt)
handleEvent dt (KeyDown (Keysym _ _ 'q')) gd = exitWith ExitSuccess
handleEvent dt SDL.Quit gd = exitWith ExitSuccess
handleEvent dt _ gd = return gd

loop :: Integer -> GameData -> SurfacesMap -> IO ()
loop startTime gameData images
    = do endTime <- getCPUTime
         let dt = (fromIntegral (endTime - startTime)) / (10^11)

         event <- pollEvent
         (GameData objects pacman) <- handleEvent dt event gameData
         let updatedObjects = moveObjects objects dt

         display (pacman : updatedObjects) images

         loop endTime (GameData updatedObjects pacman) images

main = withInit [InitVideo] $
    do screen <- setVideoMode window_width window_height 16 [SWSurface]
       setCaption "HS-Pacman" ""
       enableUnicode True
       images <- loadImages
       startTime <- getCPUTime
       loop startTime (GameData [50.0] 31.415) images
