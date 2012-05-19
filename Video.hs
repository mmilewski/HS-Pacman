module Video where

import Graphics.UI.SDL as SDL hiding (update)
import Graphics.UI.SDL.Image as Image
import Data.Map (Map, fromList)

mignore :: Monad m => m a -> m ()
mignore a = a >> return ()

---- Images
type ImagesMap = Map String SDL.Surface

loadImages :: [String] -> IO (ImagesMap)
loadImages imgsNames
    = do surfaces <- mapM (\name -> Image.load $ concat ["gfx/", name, ".png"]) imgsNames
         return $ fromList $ zip imgsNames surfaces

---- Screen
type Screen = Surface

scBlit :: Screen -> Surface -> Int -> Int -> IO ()
scBlit screen imageSurface x y = mignore $ blitSurface imageSurface Nothing screen (Just $ Rect x y 0 0)

