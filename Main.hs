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
import Ix (range)

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
img_pacman = "pacman"
img_enemy = img_placeholder
img_ball = "ball"
img_board_empty = "board-empty"
img_board_bottom = "board-bottom"
img_board_right = "board-right"
img_board_left = "board-left"
img_board_top = "board-top"
img_all = [img_ball, img_smile, img_enemy, img_pacman, img_board_empty, img_board_bottom, img_board_right, img_board_left, img_board_top]

float :: Int -> Float      -- konwersja Int ~~> Float
float = fromIntegral

mignore a = a >> return ()

loadImages :: IO (SurfacesMap)
loadImages
    = do surfaces <- mapM (\name -> Image.load $ concat ["gfx/", name, ".png"]) img_all
         return $ fromList $ zip img_all surfaces

display :: GameData -> SurfacesMap -> IO ()
display (GameData objects balls (Player plPos _) board) imagesMap
    = getVideoSurface >>= (\screen -> displayAt screen >> SDL.flip screen)
      where
        displayAt screen = displayBg >> displayBoard >> displayBalls >> displayObjects >> displayPlayer
          where
            displayBg      = mapRGB (surfaceGetPixelFormat screen) 0x1F 0x1F 0x1F >>= (\grey -> fillRect screen Nothing grey)
            displayBoard   = mapM_ (\i_p -> displayBoardPiece imagesMap i_p) $ zip (iterate (+1) 0) board
            displayBalls   = mapM_ (displayImage img_ball) balls
            displayObjects = mapM_ (displayImage img_enemy) objects
            displayPlayer  =       (displayImage img_pacman) plPos
            displayImage imgName (Vector x y) = mignore$ blitSurface (surfaceByName imgName) Nothing screen (Just $ Rect (round x) (round y) 0 0 )
            surfaceByName imgName = fromJust $ lookup imgName imagesMap
            displayBoardPiece imagesMap (i, piece) = mapM_ blit [(piece .&. 1), (piece .&. 2), (piece .&. 4), (piece .&. 8)]
                     where blit p = mignore$ blitSurface (surfaceByName (nameOf p)) Nothing screen (Just $ Rect (tileW * col) (tileH * row) 0 0)
                           nameOf p = fromJust $ lookup p $ fromList [(0, img_board_empty), (1, img_board_top), (2, img_board_right),
                                                                      (4, img_board_bottom), (8, img_board_left)]
                           (row, col) = divMod i boardWidth

data Vector = Vector Float Float
type Position = Vector
vadd (Vector a b) (Vector c d) = Vector (a + c) (b + d)
vsub (Vector a b) (Vector c d) = Vector (a - c) (b - d)
vscale (Vector a b) factor = Vector (a * factor) (b * factor)
instance Show Vector where
    show (Vector a b) = "[" ++ (show a) ++ ", " ++ (show b) ++ "]"

type Object = Vector
type Objects = [Object]
type Balls = Objects

moveObjects :: Objects -> TimeDelta -> Objects
moveObjects objects dt = map move objects
    where move (Vector x y) = Vector x' y where x' = if round x >= windowWidth then 0 else x + 20.0 * dt

data Direction = N | W | S | E | X deriving (Show, Eq)

data Player = Player { pos :: Vector,
                       dir :: Direction
                     } deriving (Show)

type Board = [Int]

brickAt :: Board -> Int -> Int -> Int
brickAt board col row = board !! ((fromIntegral$ row `div` tileH) * boardWidth + (fromIntegral$ col `div` tileW))

data GameData = GameData { objects :: Objects,
                           balls :: Balls,
                           pacman :: Player,
                           board :: Board
                         } deriving (Show)

-- -- tak napisałbym w każdym sensownym języku. Niestety Haskell nie ogarnia sytuacji
-- -- i zmusił mnie do zrobienia brzydkiego obejścia, bleh :(
-- getNearestCenter plPos@(Vector x y) = Vector x' y' where x' = (floor$ x/tileW) * tileW + tileW/2
--                                                          y' = (floor$ y/tileH) * tileH + tileH/2
getNearestCenter (Vector x y) = Vector (findCoordX x) (findCoordY y)
findCoordX :: Float -> Float
findCoordX x = if 0 <= x && x < (float tileW) then (float tileW)/2 else (float tileW) + findCoordX (x - float tileW)
findCoordY :: Float -> Float
findCoordY x = if 0 <= x && x < (float tileH) then (float tileH)/2 else (float tileH) + findCoordY (x - float tileH)

changeDir :: Player -> TimeDelta -> Direction -> Board -> Player
changeDir pacman@(Player plPos@(Vector px py) plDir) dt newDir _
    = if plDir == newDir then pacman
      else if (plDir, newDir) `elem` [(N,S), (S,N), (W,E), (E,W)] || newDir == X then pacman {dir = newDir}
      else if plDir `elem` [E, W] && 15 < (abs $ cx - px) then pacman
      else if plDir `elem` [S, N] && 15 < (abs $ cy - py) then pacman
      else pacman {dir = newDir, pos = plPos'}
           where plPos' = centeredPos
                 centeredPos@(Vector cx cy) = getNearestCenter (plPos `vadd` tileHalf) `vsub` tileHalf
                 tileHalf = Vector (float tileW) (float tileH) `vscale` 0.5

movePacman :: TimeDelta -> Player -> Direction -> Board -> Player
movePacman dt (Player pos dir) newDir board
    = Player pos' dir
      where pos' = pos `vadd` getMoveDelta dt pos newDir board
            getMoveDelta :: TimeDelta -> Position -> Direction -> Board -> Vector
            getMoveDelta dt pos@(Vector px py) dir board
                = Vector dx dy where dx = if dir `elem` [X, N, S] then 0
                                          else (case dir of
                                                   E -> if (brickAt board (        (round $ px + plSpeed*dt)) (round py) .&. 2 /= 0) then 0 else  plSpeed*dt
                                                   W -> if (brickAt board (tileW + (round $ px - plSpeed*dt)) (round py) .&. 8 /= 0) then 0 else -plSpeed*dt)
                                     dy = if dir `elem` [X, W, E] then 0
                                          else (case dir of
                                                   S -> if (brickAt board (round px) (        (round $ py + plSpeed*dt)) .&. 4 /= 0) then 0 else  plSpeed*dt
                                                   N -> if (brickAt board (round px) (tileH + (round $ py - plSpeed*dt)) .&. 1 /= 0) then 0 else -plSpeed*dt)

handleEvent :: TimeDelta -> Event -> GameData -> IO(GameData)
handleEvent dt SDL.NoEvent gd = return gd
handleEvent dt SDL.Quit gd = exitWith ExitSuccess
handleEvent dt (KeyDown (Keysym _ _ 'q')) gd = exitWith ExitSuccess
handleEvent dt (KeyDown keysym) gd@(GameData _ _ pacman board) = handleKeyDown keysym where
    updatePacmanDir dir = return $ gd{pacman = changeDir pacman dt dir board}
    handleKeyDown (Keysym SDLK_RIGHT _ _) = updatePacmanDir E
    handleKeyDown (Keysym SDLK_LEFT  _ _) = updatePacmanDir W
    handleKeyDown (Keysym SDLK_DOWN  _ _) = updatePacmanDir S
    handleKeyDown (Keysym SDLK_UP    _ _) = updatePacmanDir N
    handleKeyDown _                       = return gd
handleEvent _ _ gd = return gd

loop :: CpuTime -> GameData -> SurfacesMap -> IO ()
loop startTime gameData images
    = do endTime <- getCPUTime
         let dt = (fromIntegral (endTime - startTime)) / (10^11)

         event <- pollEvent
         (GameData objects balls pacman@(Player _ dir) board) <- handleEvent dt event gameData
         let objects' = moveObjects objects dt
         let pacman' = movePacman dt pacman dir board

         let gameData' = (GameData objects' balls pacman' board)
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
       loop startTime (GameData objects balls player board1) images
       where objects = [ (Vector 50 50), (Vector 350 150) ]
             balls = [ Vector (float x * 50.0) (float y * 50.0) | x <- range (0, 12-1), y <- range(0, 7-1) ]
             player = (Player (Vector 0 0) X)
             t=1; r=2; b=4; l=8
             tl=t+l; tr=t+r; bl=b+l; br=b+r; lr=l+r; tb=t+b

             board1 = [tl,  t, tb, tb,  t, tb, tb, tb, tr, tl,  t, tr,  t,  t,  t, tr,
                       lr, lr, tl, tb,  0, tb, tb, tb,  0, br, lr, lr,  0,  0,  0,  r,
                       lr, bl,  r, tl, br, tl,  t, tr,  l, tr, lr, lr,  0,  0,  0,  r,
                        l, tb,  r, lr, tl, br, lr, lr, bl, br, lr, lr,  0,  0,  0,  r,
                        l, tb, br, lr, bl, tr, lr, lr, tl, tb, br, lr,  0,  0,  0,  r,
                        l, tb, tr, bl, tr, lr, lr, bl,  r, tl, tr, lr,  0,  0,  0,  r,
                       bl, tb,  b, tb,  b, br, bl, tb, br, bl,  b, br,  0,  0,  0,  r,
                       tl,  t,  t,  t,  t,  t,  t,  t,  t,  t,  t,  t,  t,  t,  t, tr,
                        l,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  r,
                        l,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  r,
                        l,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  r,
                       bl,  b,  b,  b,  b,  b,  b,  b,  b,  b,  b,  b,  b,  b,  b, br]
             board = [ 9,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  3,
                       8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2,
                       8,  0,  0,  4,  2,  8,  0,  0,  0,  0,  0,  1,  3,  0,  0,  2,
                       8,  0,  2, 15, 10,  8,  0,  0,  2,  0,  0,  0,  2,  0,  0,  2,
                       8,  0,  0,  1,  2,  8,  0,  0,  2,  0,  0,  0,  2,  0,  0,  2,
                       8,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  2,  0,  0,  2,
                       8,  0,  0,  2,  0,  2,  0,  0,  2,  0, 12,  4,  6,  4,  6,  2,
                       8,  2,  8,  2,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2,
                       8,  0, 12,  6,  4,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2,
                       8,  0,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  2,
                       8,  0,  0,  0,  2,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  2,
                      12,  4,  4,  4,  6,  4,  4,  4,  4,  6,  4,  4,  4,  4,  4,  6]
