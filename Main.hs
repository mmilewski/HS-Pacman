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

type Object = Vector
type Objects = [Object]
type Screen = Surface
type SurfacesMap = Map String Surface
type TimeDelta = Float
type CpuTime = Integer

(windowWidth, windowHeight) = (800, 600)
(boardWidth, boardHeight) = (16, 12)
boardSize = (boardWidth, boardHeight)
brickWidth  = windowWidth `div` boardWidth
brickHeight = windowHeight `div` boardHeight

img_placeholder = img_smile
img_smile = "smile"
img_pacman = img_smile --"pacman"
img_board_empty = "board-empty"
img_board_bottom = "board-bottom"
img_board_right = "board-right"
img_board_left = "board-left"
img_board_top = "board-top"
img_all = [img_placeholder, img_pacman, img_board_empty, img_board_bottom, img_board_right, img_board_left, img_board_top]

loadImages :: IO (SurfacesMap)
loadImages
    = do surfaces <- mapM (\name -> Image.load $ concat ["gfx/", name, ".png"]) img_all
         return $ fromList $ zip img_all surfaces

displayObject :: Screen -> Vector -> Surface -> IO ()
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
               surfaceByPiece p = fromJust $ lookup (fromJust $ lookup p boardMap) imagesMap
               boardMap = fromList [(0, img_board_empty), (1, img_board_top), (2, img_board_right), (4, img_board_bottom), (8, img_board_left)]
               (tileW, tileH) = (50, 50)
               row = fromIntegral $ i `div` boardWidth
               col = fromIntegral $ i `mod` boardWidth

display :: GameData -> SurfacesMap -> IO ()
display (GameData objects (Player plPos _) board) imagesMap =
  do screen <- getVideoSurface
     green <- mapRGB (surfaceGetPixelFormat screen) 0 0xFF 0
     fillRect screen Nothing green
     mapM_ (\ip -> displayBoardPiece screen imagesMap boardSize ip) $ zip (List.iterate (+1) 0) board
     let placeholder = fromJust $ lookup img_placeholder imagesMap
     mapM_ (\objPos -> displayObject screen objPos placeholder) objects
     displayObject screen plPos (fromJust $ lookup img_pacman imagesMap)
     SDL.flip screen

moveObjects :: Objects -> TimeDelta -> Objects
moveObjects objects dt
    = map move objects
      where move (Vector x y) = Vector x' y
              where x' = if x > fromIntegral windowWidth then 0 else x + 20.0 * dt

data Vector = Vector Float Float
type Position = Vector
vadd (Vector a b) (Vector c d) = Vector (a + c) (b + d)
vscale (Vector a b) factor = Vector (a * factor) (b * factor)
instance Show Vector where
    show (Vector a b) = "[" ++ (show a) ++ ", " ++ (show b) ++ "]"


data PlayerPosUpdater = Updater { update :: Position -> TimeDelta -> Board -> (Position, PlayerPosUpdater) }
asUpdater fun = Updater fun

data Player = Player { pos :: Position,
                       posUpdater :: PlayerPosUpdater
                     }

type Board = [Int]
data GameData = GameData { objects :: Objects,
                           pacman :: Player,
                           board :: Board
                         }

{-
posx' <- posx + velx * dt

w lewo
   można iść dopóki  brick(posx', posy) == brick(posx, posy)

w prawo
   można iść dopóki  brick(posx'+brickW, posy) == brick(posx+brickW, posy)
-}
brickAt board x y = List.head $ List.drop n board
    where n = (round $ fromIntegral $ y `div` brickHeight) * boardWidth + (round $ fromIntegral $ x `div` brickWidth)

movePlayerRight :: Position -> TimeDelta -> Board -> (Position, PlayerPosUpdater)
movePlayerRight oldPos dt board = (vadd oldPos (Vector ( 40.0 * dt) 0), asUpdater movePlayerRight)
movePlayerLeft  oldPos dt board = (vadd oldPos (Vector (-40.0 * dt) 0), asUpdater movePlayerLeft)
movePlayerDown  oldPos dt board = (vadd oldPos (Vector 0 ( 40.0 * dt)), asUpdater movePlayerDown)
movePlayerUp    oldPos dt board = (vadd oldPos (Vector 0 (-40.0 * dt)), asUpdater movePlayerUp)
dontMove oldPos _ _ = (oldPos, Updater dontMove)

handleEvent :: TimeDelta -> Event -> GameData -> IO(GameData)
handleEvent _  SDL.Quit    _  = exitWith ExitSuccess
handleEvent _  SDL.NoEvent gd = return gd
handleEvent _  (KeyDown (Keysym _ _ 'q')) _ = exitWith ExitSuccess
handleEvent dt (KeyDown keysym) gd@(GameData objs (Player plPos _) board) = keyDown keysym where
    updateWithFun updateFun = return $ GameData objs (Player plPos $ asUpdater updateFun) board
    keyDown (Keysym SDLK_RIGHT _ _) = updateWithFun movePlayerRight
    keyDown (Keysym SDLK_LEFT  _ _) = updateWithFun movePlayerLeft
    keyDown (Keysym SDLK_DOWN  _ _) = updateWithFun movePlayerDown
    keyDown (Keysym SDLK_UP    _ _) = updateWithFun movePlayerUp
    keyDown _ = return gd
handleEvent _ _ gd = return gd

loop :: CpuTime -> GameData -> SurfacesMap -> IO ()
loop startTime gameData images
    = do endTime <- getCPUTime
         let dt = (fromIntegral (endTime - startTime)) / (10^11)

         event <- pollEvent
         (GameData objects (Player plPos (Updater plUpdater)) board) <- handleEvent dt event gameData
         let objects' = moveObjects objects dt
         let (plPos', plUpdater') = plUpdater plPos dt board
         let pacman' = Player plPos' plUpdater'

         let gameData' = (GameData objects' pacman' board)
         display gameData' images
         loop endTime gameData' images

assert :: Show a => Eq a => a -> a -> String -> IO()
assert expected actual msg = if expected == actual then return () else error $ "\nexpected: " ++ (show expected) ++ "\nactual: " ++ (show actual) ++ "\ndata: " ++ msg

nums = iterate (+1) 0

main = withInit [InitVideo] $
    do screen <- setVideoMode windowWidth windowHeight 16 [SWSurface]

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
