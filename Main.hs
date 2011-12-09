module Main where

import Graphics.UI.SDL as SDL

import Foreign
import Data.Typeable
import Data.Char

import System.Environment
import System.Exit
import System.Random

import System.Posix as P  -- usleep

import Data.List as List
import Data.Map as Map

img_smile = "smile"
img_pacman = "pacman"

(window_width, window_height) = (800, 600)


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
       setCaption "Test" ""
       enableUnicode True
       images <- loadImages
       loop (display $ List.map snd $ Map.toList images)

display_img :: Surface -> Surface -> IO ()
display_img screen image =
  do posX <- randomRIO (300-100, 300+100)
     posY <- randomRIO (300-100, 300+100)
     blitSurface image Nothing screen (Just (Rect posX posY 0 0))
     return ()

display images@(h:_) = do screen <- getVideoSurface
                          let format = surfaceGetPixelFormat screen in
                             do red   <- mapRGB format 0xFF 0 0
                                green <- mapRGB format 0 0xFF 0
                                fillRect screen Nothing green
                                fillRect screen (Just (Rect 10 10 10 10)) red
                                mapM_ (\img -> display_img screen img) images
                                SDL.flip screen

loop :: IO () -> IO ()
loop display
    = do event <- pollEvent -- waitEvent
         case event of
           SDL.Quit -> exitWith ExitSuccess
           KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess
           KeyDown (Keysym _ _ ' ') -> return () -- display
           _ -> return ()
         usleep 100000
         display
         loop display

