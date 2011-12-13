module Main where

import Graphics.UI.SDL as SDL

-- import Foreign
-- import Data.Typeable
-- import Data.Char

-- import System.Environment
import System.Exit
import System.Random

import System.Posix as P  -- usleep
import System.CPUTime

import Data.List as List
import Data.Map as Map

img_smile = "smile"
img_pacman = "pacman"

(window_width, window_height) = (800, 600)

loadImages :: IO (Map String Surface)
loadImages = let ext = ".bmp"
                 loadImg name = loadBMP $ name ++ ext
             in do smile <- loadImg $ img_smile
                   pacman <- loadImg $ img_pacman
                   return $ Map.fromList [
                                           (img_pacman, pacman),
                                           (img_smile, smile)
                                         ]

main = withInit [InitVideo] $
    do screen <- setVideoMode window_width window_height 16 [SWSurface]
       setCaption "HS-Pacman" ""
       enableUnicode True
       images <- loadImages
       startTime <- getCPUTime
       loop startTime [50.0] images

displayObjects :: Surface -> Float -> [Surface] -> IO ()
displayObjects screen posx images
    = do blitSurface (List.head images)  Nothing screen (Just (Rect (round posx) 50 0 0) )
         return ()

display :: [Float] -> Map String Surface -> IO ()
display objects imagesMap =
  do let images = List.map snd $ Map.toList imagesMap
     screen <- getVideoSurface
     let format = surfaceGetPixelFormat screen in
       do red   <- mapRGB format 0xFF 0 0
          green <- mapRGB format 0 0xFF 0
          fillRect screen Nothing green
          fillRect screen (Just (Rect 10 10 10 10)) red
          mapM_ (\obj -> displayObjects screen obj images) objects
       -- mapM_ (\img -> display_img screen img) images
          SDL.flip screen

handleQuitEvents :: IO ()
handleQuitEvents
    = do event <- pollEvent -- waitEvent
         case event of
           SDL.Quit -> exitWith ExitSuccess
           KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess
           KeyDown (Keysym _ _ ' ') -> return () -- display
           _ -> return ()

updateObjects :: [Float] -> Float -> IO ([Float])
updateObjects objects dt
    = mapM updateObject objects
      where updateObject obj = return $ ( if obj > (fromIntegral window_width) then 0 else obj + 20.0 * dt)

loop :: Integer -> [Float] -> Map String Surface -> IO ()
loop startTime objects images
    = do handleQuitEvents
         endTime <- getCPUTime
         -- putStrLn $ "Time: " ++ show endTime
         let dt = (fromIntegral (endTime - startTime)) / (10^11)
         updatedObjects <- updateObjects objects dt
         display updatedObjects images
         loop endTime updatedObjects images
