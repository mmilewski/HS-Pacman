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

(width, height) = (800, 600)


loadImages = let ext = ".bmp"
                 loadImg name = loadBMP $ name ++ ext
             in do image <- loadImg $ img_smile
                   return $ Map.fromList [ (img_smile, image)
                                         ]

main = withInit [InitVideo] $
    do screen <- setVideoMode width height 16 [SWSurface]
       setCaption "Test" ""
       enableUnicode True
       images <- loadImages
       let image = images Map.! img_smile in
         do display image
            loop (display image)

display :: Surface -> IO ()
display image
    = do screen <- getVideoSurface
         let format = surfaceGetPixelFormat screen
         red   <- mapRGB format 0xFF 0 0
         green <- mapRGB format 0 0xFF 0
         fillRect screen Nothing green
         fillRect screen (Just (Rect 10 10 10 10)) red
         posX <- randomRIO (100-20, 100+20)
         posY <- randomRIO (100-10, 100+10)
         blitSurface image Nothing screen (Just (Rect posX posY 0 0))
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

