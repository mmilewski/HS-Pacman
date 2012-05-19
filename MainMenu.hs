module MainMenu where

import Graphics.UI.SDL as SDL hiding (update)
import System.Exit
import Data.Maybe (fromJust)
import System.CPUTime (getCPUTime)
import Prelude hiding (lookup)   -- Haskell sucks, sometimes
import Data.Map (lookup)

import Video (scBlit, ImagesMap)
import ImageDefs
import Vector
import GameState

data MainMenuData = MainMenuData { selectedButtonId :: Int }
                  | MainMenuPlay deriving (Show)

defaultMainMenuData :: MainMenuData
defaultMainMenuData = MainMenuData 0

mmUpdate :: MainMenuData -> IO(MainMenuData)
mmUpdate mmData@(MainMenuData selectedButton)
    = do event <- pollEvent
         moveSelection event
         where moveSelection SDL.Quit = exitWith ExitSuccess
               moveSelection (KeyDown (Keysym _ _ 'q'))       = exitWith ExitSuccess
               moveSelection (KeyDown (Keysym SDLK_RETURN _ _)) = if selectedButton == 0 then startGame else exitWith ExitSuccess
               moveSelection (KeyDown (Keysym SDLK_DOWN _ _)) = return$ mmData {selectedButtonId = 1}
               moveSelection (KeyDown (Keysym SDLK_UP   _ _)) = return$ mmData {selectedButtonId = 0}
               moveSelection _ = return$ mmData
               startGame = return MainMenuPlay

mmDisplay :: MainMenuData -> ImagesMap -> IO ()
mmDisplay (MainMenuData selectedButton) imagesMap
    = getVideoSurface >>= (\screen -> displayAt screen >> SDL.flip screen)
      where
        displayAt screen = displayBg >> displayPlayButton >> displayQuitButton
          where
            displayBg         = mapRGB (surfaceGetPixelFormat screen) 0x1F 0x1F 0x1F >>= (\grey -> fillRect screen Nothing grey)
            displayPlayButton = displayImage (img_btn_play!!selectedButton) (Vector 200 150)
            displayQuitButton = displayImage (img_btn_quit!!((selectedButton+1)`mod`2)) (Vector 200 250)

            displayImage imgName (Vector x y) = scBlit screen (surfaceByName imgName) (round x) (round y)
            surfaceByName imgName = fromJust$ lookup imgName imagesMap

mmLoop :: MainMenuData -> ImagesMap -> IO(GameState)
mmLoop mmData images
    = do mmData' <- mmUpdate mmData
         case mmData' of
           MainMenuPlay   -> return GSPlay
           MainMenuData _ -> do mmDisplay mmData images
                                mmLoop mmData' images
