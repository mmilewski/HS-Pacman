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
(tileW, tileH) = (windowWidth `div` boardWidth, windowHeight `div` boardHeight)

plSpeed = 20         -- player/pacman's speed (px/s)

img_placeholder = img_smile
img_smile = "smile"
img_pacman = img_smile --"pacman"
img_board_empty = "board-empty"
img_board_bottom = "board-bottom"
img_board_right = "board-right"
img_board_left = "board-left"
img_board_top = "board-top"
img_all = [img_smile, img_pacman, img_board_empty, img_board_bottom, img_board_right, img_board_left, img_board_top]

roundFromIntegral x = round $ fromIntegral x
float :: Int -> Float      -- konwersja Int ~~> Float
float = fromIntegral

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
vsub (Vector a b) (Vector c d) = Vector (a - c) (b - d)
vscale (Vector a b) factor = Vector (a * factor) (b * factor)
instance Show Vector where
    show (Vector a b) = "[" ++ (show a) ++ ", " ++ (show b) ++ "]"

type Object = Vector
type Objects = [Object]

moveObjects :: Objects -> TimeDelta -> Objects
moveObjects objects dt = map move objects
    where move (Vector x y) = Vector x' y where x' = if round x >= windowWidth then 0 else x + 20.0 * dt

data Direction = N | W | S | E | X deriving (Show, Eq, Ord)

data Player = Player { pos :: Vector,
                       dir :: Direction
                     } deriving (Show)



type Board = [Int]
data GameData = GameData { objects :: Objects,
                           pacman :: Player,
                           board :: Board
                         } deriving (Show)

{-
posx' <- posx + velx * dt

w lewo
   można iść dopóki  brick(posx', posy) == brick(posx, posy)

w prawo
   można iść dopóki  brick(posx'+tileW, posy) == brick(posx+tileW, posy)
-}
brickAt :: Board -> Int -> Int -> Int
brickAt board x y = List.head $ List.drop n board
    where n = (roundFromIntegral$ y `div` tileH) * boardWidth + (roundFromIntegral$ x `div` tileW)


-- -- tak napisałbym w każdym sensownym języku. Niestety Haskell nie ogarnia sytuacji
-- -- i zmusił mnie do zrobienia brzydkiego obejścia, bleh :(
-- getNearestCenter plPos@(Vector x y) = Vector x' y' where x' = (floor$ x/tileW) * tileW + tileW/2
--                                                          y' = (floor$ y/tileH) * tileH + tileH/2
getNearestCenter plPos@(Vector x y) = Vector x' y' where x' = findCoordX x
                                                         y' = findCoordY y
findCoordX :: Float -> Float
findCoordX x = if 0 <= x && x < (float tileW) then (float tileW)/2 else (float tileW) + findCoordX (x - float tileW)
findCoordY :: Float -> Float
findCoordY x = if 0 <= x && x < (float tileH) then (float tileH)/2 else (float tileH) + findCoordY (x - float tileH)

changeDir :: Player -> TimeDelta -> Direction -> Board -> Player
changeDir pacman@(Player plPos plDir) dt newDir board
    = if plDir == newDir then pacman
      else pacman {dir = newDir, pos = plPos'}
           where plPos' = getNearestCenter plPos `vsub` tileHalf
                 tileHalf = Vector (float tileW) (float tileH) `vscale` 0.5

movePacman :: TimeDelta -> Player -> Direction -> Board -> Player
movePacman dt (Player pos dir) newDir board
    = Player pos' dir
      where pos' = (pos `vadd` (offset `vscale` dt))
            offset = case newDir of
              N -> Vector 0          (-plSpeed)
              S -> Vector 0          plSpeed
              W -> Vector plSpeed    0
              E -> Vector (-plSpeed) 0
              X -> Vector 0          0

handleEvent :: TimeDelta -> Event -> GameData -> IO(GameData)
handleEvent dt SDL.NoEvent gd = return gd
handleEvent dt SDL.Quit gd = exitWith ExitSuccess
handleEvent dt (KeyDown (Keysym _ _ 'q')) gd = exitWith ExitSuccess
handleEvent dt (KeyDown keysym) gd@(GameData _ pacman board) = handleKeyDown keysym where
    handleKeyDown (Keysym SDLK_RIGHT _ _) = return $ gd{pacman = changeDir pacman dt W board}
    handleKeyDown (Keysym SDLK_LEFT  _ _) = return $ gd{pacman = changeDir pacman dt E board}
    handleKeyDown (Keysym SDLK_DOWN  _ _) = return $ gd{pacman = changeDir pacman dt S board}
    handleKeyDown (Keysym SDLK_UP    _ _) = return $ gd{pacman = changeDir pacman dt N board}
    handleKeyDown _                       = return gd
handleEvent _ _ gd = return gd

loop :: CpuTime -> GameData -> SurfacesMap -> IO ()
loop startTime gameData images
    = do endTime <- getCPUTime
         let dt = (fromIntegral (endTime - startTime)) / (10^11)

         event <- pollEvent
         (GameData objects pacman@(Player _ dir) board) <- handleEvent dt event gameData
         let objects' = moveObjects objects dt
         let pacman' = movePacman dt pacman dir board

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
             player = (Player (Vector 50 100) X)
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
