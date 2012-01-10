module Main where

import Prelude hiding (lookup)
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import System.Exit
import System.CPUTime
import Data.List as List hiding (lookup)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromJust)
import Data.Bits ((.&.))

type SurfacesMap = Map String Surface
type TimeDelta = Float
type CpuTime = Integer
type Screen = Surface


(windowWidth, windowHeight) = (800, 600)       -- rozmiar okna w px
(boardWidth, boardHeight) = (16, 12)           -- ilosc kafli w poziomie i pionie
boardSize = (boardWidth, boardHeight)
(tileW, tileH) = (roundFromIntegral $ windowWidth `div` boardWidth, roundFromIntegral $ windowHeight `div` boardHeight)   -- rozmiar kafla w px

img_placeholder = img_smile
img_smile = "smile"
img_pacman = img_smile --"pacman"
img_board_empty = "board-empty"
img_board_bottom = "board-bottom"
img_board_right = "board-right"
img_board_left = "board-left"
img_board_top = "board-top"
img_all = [img_placeholder, img_pacman, img_board_empty, img_board_bottom, img_board_right, img_board_left, img_board_top]

roundFromIntegral x = round $ fromIntegral x

loadImages :: IO (SurfacesMap)
loadImages
    = do surfaces <- mapM (\name -> Image.load $ concat ["gfx/", name, ".png"]) img_all
         return $ fromList $ zip img_all surfaces

display :: GameData -> SurfacesMap -> IO ()
display (GameData objects (Player plPos _) board) imagesMap
    = do screen <- getVideoSurface
         displayBg screen
         displayBoard screen
         displayObjects screen
         displayPlayer screen
         SDL.flip screen
      where
        displayBg screen = mapRGB (surfaceGetPixelFormat screen) 0x1F 0x1F 0x1F >>= (\grey -> fillRect screen Nothing grey)
        displayBoard screen = mapM_ (\i_p -> displayBoardPiece screen imagesMap boardSize i_p) $ zip (iterate (+1) 0) board
        displayObjects screen = do let placeholder = fromJust $ lookup img_placeholder imagesMap
                                   mapM_ (\objPos -> displayObject screen objPos placeholder) objects
        displayPlayer screen = displayObject screen plPos (fromJust $ lookup img_pacman imagesMap)

        displayObject :: Screen -> Position -> Surface -> IO ()
        displayObject screen (Vector x y) image = do blitSurface image Nothing screen (Just $ Rect (round x) (round y) 0 0 ) >> return ()
        displayBoardPiece :: Surface -> SurfacesMap -> (Int, Int) -> (Int, Int) -> IO ()
        displayBoardPiece screen imagesMap (boardWidth, boardHeight) (i, piece)
            = do blit $ surfaceByPiece (piece .&. 1)
                 blit $ surfaceByPiece (piece .&. 2)
                 blit $ surfaceByPiece (piece .&. 4)
                 blit $ surfaceByPiece (piece .&. 8)
                 where blit surface = blitSurface surface Nothing screen (Just $ Rect (tileW * col) (tileH * row) 0 0) >> return ()
                       surfaceByPiece p = fromJust $ lookup (fromJust $ lookup p boardMap) imagesMap
                       boardMap = fromList [(0, img_board_empty), (1, img_board_top), (2, img_board_right), (4, img_board_bottom), (8, img_board_left)]
                       row = fromIntegral $ i `div` boardWidth
                       col = fromIntegral $ i `mod` boardWidth

data Vector = Vector Float Float
type Position = Vector
vadd (Vector a b) (Vector c d) = Vector (a + c) (b + d)
vscale (Vector a b) factor = Vector (a * factor) (b * factor)
instance Show Vector where
    show (Vector a b) = "[" ++ (show a) ++ ", " ++ (show b) ++ "]"

type Object = Vector
type Objects = [Object]

moveObjects :: Objects -> TimeDelta -> Objects
moveObjects objects dt = map move objects
    where move (Vector x y) = Vector x' y where x' = if round x >= windowWidth then 0 else x + 20.0 * dt

type UpdaterType = Position -> TimeDelta -> Board -> (Position, PlayerPosUpdater)
data PlayerPosUpdater = Updater { update :: UpdaterType }
asUpdater fun = Updater fun

data Player = Player { pos :: Position,
                       posUpdater :: PlayerPosUpdater
                     }

type Board = [Int]
data GameData = GameData { objects :: Objects,
                           pacman :: Player,
                           board :: Board
                         }

gd_changePlUpdater :: GameData -> UpdaterType -> GameData
gd_changePlUpdater gd@GameData{pacman=p} newUpdateFun = gd { pacman = p {posUpdater = asUpdater newUpdateFun} }

{-
posx' <- posx + velx * dt

w lewo
   można iść dopóki  brick(posx', posy) == brick(posx, posy)

w prawo
   można iść dopóki  brick(posx'+brickW, posy) == brick(posx+brickW, posy)
-}
brickAt board x y = List.head $ List.drop n board
    where n = (roundFromIntegral $ y `div` tileH) * boardWidth + (roundFromIntegral $ x `div` tileW)

movePlayerRight oldPos@(Vector x y) dt board = (oldPos `vadd` Vector ( 40.0 * dt) 0,  asUpdater $ if (round x) + tileW > windowWidth  then movePlayerLeft  else movePlayerRight)
movePlayerLeft  oldPos@(Vector x y) dt board = (oldPos `vadd` Vector (-40.0 * dt) 0,  asUpdater $ if (round x) <= 0                   then movePlayerRight else movePlayerLeft)
movePlayerDown  oldPos@(Vector x y) dt board = (oldPos `vadd` Vector 0 ( 40.0 * dt),  asUpdater $ if (round y) + tileH > windowHeight then movePlayerUp    else movePlayerDown)
movePlayerUp    oldPos@(Vector x y) dt board = (oldPos `vadd` Vector 0 (-40.0 * dt),  asUpdater $ if (round y) <= 0                   then movePlayerDown  else movePlayerUp)
dontMove oldPos _ _ = (oldPos, asUpdater dontMove)


handleEvent :: Event -> GameData -> IO(GameData)
handleEvent (KeyDown (Keysym _ _ 'q')) _  = exitWith ExitSuccess
handleEvent SDL.Quit                   _  = exitWith ExitSuccess
handleEvent SDL.NoEvent                gd = return gd
handleEvent (KeyDown keysym)           gd = keyDown keysym where
    setPlUpdater f = return $ gd_changePlUpdater gd f
    keyDown (Keysym SDLK_RIGHT _ _) = setPlUpdater movePlayerRight
    keyDown (Keysym SDLK_LEFT  _ _) = setPlUpdater movePlayerLeft
    keyDown (Keysym SDLK_DOWN  _ _) = setPlUpdater movePlayerDown
    keyDown (Keysym SDLK_UP    _ _) = setPlUpdater movePlayerUp
    keyDown _ = return gd
handleEvent _                          gd = return gd

loop :: CpuTime -> GameData -> SurfacesMap -> IO ()
loop startTime gameData images
    = do endTime <- getCPUTime
         let dt = (fromIntegral (endTime - startTime)) / (10^11)

         event <- pollEvent
         (GameData objects (Player plPos (Updater plUpdater)) board) <- handleEvent event gameData
         let objects' = moveObjects objects dt
         let (plPos', plUpdater') = plUpdater plPos dt board
         let pacman' = Player plPos' plUpdater'

         let gameData' = (GameData objects' pacman' board)
         display gameData' images
         loop endTime gameData' images

assert :: Show a => Eq a => a -> a -> String -> IO()
assert expected actual msg = if expected == actual then return () else error $ concat ["\nexpected: ", (show expected), "\nactual: ", (show actual), "\ndata: ", msg]

main = withInit [InitVideo] $
    do screen <- setVideoMode windowWidth windowHeight 16 [SWSurface]

       let nums = iterate (+1) 0
       mapM_ (\(x,y) -> assert 0 (brickAt nums x y) (show x ++ " , " ++ show y)) $ zip (take 50 $ drop (0*50) nums) (take 50 nums)
       mapM_ (\(x,y) -> assert 1 (brickAt nums x y) (show x ++ " , " ++ show y)) $ zip (take 50 $ drop (1*50) nums) (take 50 nums)
       mapM_ (\(x,y) -> assert 5 (brickAt nums x y) (show x ++ " , " ++ show y)) $ zip (take 50 $ drop (5*50) nums) (take 50 nums)
       mapM_ (\(x,y) -> assert (0+1*16) (brickAt nums x y) (show x ++ " , " ++ show y)) $ zip (take 50 $ drop (0*50) nums) (take 50 $ drop(1*50) $ nums)
       mapM_ (\(x,y) -> assert (1+1*16) (brickAt nums x y) (show x ++ " , " ++ show y)) $ zip (take 50 $ drop (1*50) nums) (take 50 $ drop(1*50) $ nums)
       mapM_ (\(x,y) -> assert (5+1*16) (brickAt nums x y) (show x ++ " , " ++ show y)) $ zip (take 50 $ drop (5*50) nums) (take 50 $ drop(1*50) $ nums)
       mapM_ (\(x,y) -> assert (0+10*16) (brickAt nums x y) (show x ++ " , " ++ show y)) $ zip (take 50 $ drop (0*50) nums) (take 50 $ drop(10*50) $ nums)
       mapM_ (\(x,y) -> assert (1+10*16) (brickAt nums x y) (show x ++ " , " ++ show y)) $ zip (take 50 $ drop (1*50) nums) (take 50 $ drop(10*50) $ nums)
       mapM_ (\(x,y) -> assert (5+10*16) (brickAt nums x y) (show x ++ " , " ++ show y)) $ zip (take 50 $ drop (5*50) nums) (take 50 $ drop(10*50) $ nums)


       setCaption "HS-Pacman" ""
       enableUnicode True
       images <- loadImages
       startTime <- getCPUTime
       loop startTime (GameData objects player board) images
       where objects = [ (Vector 50 50), (Vector 350 150) ]
             player = (Player (Vector 0 25) (asUpdater dontMove))
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
