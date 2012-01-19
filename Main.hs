{-
   Pacman in Haskell using SDL library

   by Marcin Milewski, 2012

Wymagane biblioteki:
  * SDL:       libghc6-sdl-dev
  * SDL_image: libghc6-sdl-image-dev

Zasady/Opis:
  1. Gra polega na zebraniu wszystkich białych kulek znajdujących się na planszy.
  2. Gracz steruje Pacmanem przy pomocy strzałek na klawiaturze.
  3. W grze obecne są duchy, które gonią Pacmana. Bliski kontakt z nimi oznacza zakończenie rozgrywki.
  4. Jeżeli gracz weźmie bonus w postaci papryki, wtedy role duchów i Pacmana odwracają się,
     tzn. w przypadku spotkania duch zostaje usunięty z planszy.
  5. Zarówno gracz jak i duchy nie mogą przechodzić przez ściany.
-}
module Main where

import Prelude hiding (lookup)
import Graphics.UI.SDL as SDL hiding (update)
import Graphics.UI.SDL.Image as Image
import System.Exit
import System.CPUTime
import Data.List as List hiding (lookup)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromJust)
import Data.Bits ((.&.))
import Ix (range)

type TimeDelta = Float
type Speed = Float
type CpuTime = Integer

(windowWidth, windowHeight) = (800, 600)
(boardWidth,  boardHeight)  = (16, 12)         -- ilosc kafli w poziomie i pionie
boardSize = (boardWidth, boardHeight)
(tileW, tileH) = (windowWidth `div` boardWidth, windowHeight `div` boardHeight)
tileHalf = Vector (float tileW) (float tileH) `vscale` 0.5

img_pacman = "pacman"
img_pacman_hunting = "pacman_hunting"
img_enemy = "enemy"
img_ball = "ball"
img_fruit = "fruit"
img_board_empty = "board-empty"
img_board_bottom = "board-bottom"
img_board_right = "board-right"
img_board_left = "board-left"
img_board_top = "board-top"
img_all = [img_fruit, img_ball, img_enemy, img_pacman, img_pacman_hunting,
           img_board_empty, img_board_bottom, img_board_right, img_board_left, img_board_top]

float :: Int -> Float      -- konwersja Int ~~> Float
float = fromIntegral

notNull :: [a] -> Bool
notNull = not . List.null

mignore :: Monad m => m a -> m ()
mignore a = a >> return ()

---- Images
type ImagesMap = Map String Surface

loadImages :: IO (ImagesMap)
loadImages
    = do surfaces <- mapM (\name -> Image.load $ concat ["gfx/", name, ".png"]) img_all
         return $ fromList $ zip img_all surfaces

---- Screen
type Screen = Surface

scBlit :: Screen -> Surface -> Int -> Int -> IO ()
scBlit screen imageSurface x y = mignore $ blitSurface imageSurface Nothing screen (Just $ Rect x y 0 0)

---- Direction
data Direction = N | W | S | E | X deriving (Show, Eq)

---- Vector
data Vector = Vector Float Float deriving (Eq)

type Position = Vector

vadd (Vector a b) (Vector c d) = Vector (a + c) (b + d)

vsub (Vector a b) (Vector c d) = Vector (a - c) (b - d)

vlen (Vector a b) = sqrt $ (a*a + b*b)

vscale (Vector a b) factor = Vector (a * factor) (b * factor)

instance Show Vector where
    show (Vector a b) = "[" ++ (show a) ++ ", " ++ (show b) ++ "]"

---- Board
type Board = [Int]

brickAt :: Board -> Int -> Int -> Int
brickAt board col row = board !! n where n = (fromIntegral$ row `div` tileH) * boardWidth + (fromIntegral$ col `div` tileW)

getNearestCenter (Vector x y) = Vector x' y' where x' = (float.floor $ x/tw) * tw + tw/2
                                                   y' = (float.floor $ y/th) * th + th/2
                                                   (tw, th) = (float tileW, float tileH)

defaultMove :: TimeDelta -> Position -> Direction -> Board -> Speed -> Position
defaultMove dt pos@(Vector px py) dir board speed
    = pos `vadd` Vector dx dy
      where dx = if dir `elem` [X, N, S] then 0
                 else (case dir of
                          E -> if brickAt board (        (round $ px + speed*dt)) (round py) .&. 2 /= 0 then 0 else  speed*dt
                          W -> if brickAt board (tileW + (round $ px - speed*dt)) (round py) .&. 8 /= 0 then 0 else -speed*dt)
            dy = if dir `elem` [X, W, E] then 0
                 else (case dir of
                          S -> if brickAt board (round px) (        (round $ py + speed*dt)) .&. 4 /= 0 then 0 else  speed*dt
                          N -> if brickAt board (round px) (tileH + (round $ py - speed*dt)) .&. 1 /= 0 then 0 else -speed*dt)

---- Ball
type Ball = Position
type Balls = [Ball]

makeBall :: Position -> Ball
makeBall pos = pos

---- Fruit
type Fruit = Position
type Fruits = [Fruit]

makeFruit :: Position -> Fruit
makeFruit pos = pos

---- Enemy
data Enemy = Enemy { enPos :: Position,           -- top left corner
                     enDir :: Direction,
                     enIsHunting :: Bool
                   } deriving (Show)
type Enemies = [Enemy]

enChaseSpeed = plSpeed * 0.8
enEscapeSpeed = plSpeed * 0.4

makeEnemy :: Position -> Enemy
makeEnemy pos = Enemy pos X True

enGetPos :: Enemy -> Position
enGetPos Enemy{enPos=p} = p

enMakeScared  enemy = enemy{enIsHunting=False}
enMakeHunting enemy = enemy{enIsHunting=True}

enUpdate :: Enemy -> TimeDelta -> Player -> Board -> Enemy
enUpdate enemy@(Enemy pos dir isHunting) dt pacman board
    = if canChangeDir then (if isHunting then minimumBy else maximumBy) cmp [ew,ee,es,en] else enMove enemy dt board
      where ew = enMove (snapY$ enemy{enDir=W}) dt board
            ee = enMove (snapY$ enemy{enDir=E}) dt board
            es = enMove (snapX$ enemy{enDir=S}) dt board
            en = enMove (snapX$ enemy{enDir=N}) dt board
            distToPlayer e = vlen $ (enGetPos e) `vsub` (plGetPos pacman)
            cmp e e' | distToPlayer e < distToPlayer e' = LT
                     | distToPlayer e > distToPlayer e' = GT
                     | True  = EQ
            snapX enemy@(Enemy{enPos=Vector _ py}) = enemy{enPos=Vector cx py}
            snapY enemy@(Enemy{enPos=Vector px _}) = enemy{enPos=Vector px cy}
            centeredPos@(Vector cx cy) = getNearestCenter (pos `vadd` tileHalf) `vsub` tileHalf
            canChangeDir = vlen (pos `vsub` centeredPos) < snapDistance
            snapDistance = 5

enMove :: Enemy -> TimeDelta -> Board -> Enemy
enMove enemy@(Enemy pos dir isHunting) dt board
    = enemy{enPos = defaultMove dt pos dir board speed}
      where speed = if isHunting then enChaseSpeed else enEscapeSpeed

---- Player
data Player = Player { plPos :: Position,           -- top left corner
                       plDir :: Direction,
                       plHuntingTime :: TimeDelta   -- time left to upgrade deactivation
                     } deriving (Show)

plSpeed = 10*100 :: Speed    -- player/pacman's speed (px/s)

makePlayer :: Position -> Player
makePlayer pos = Player pos X 0.0

plGetPos :: Player -> Position
plGetPos Player{plPos=p} = p

plMakeHunting pacman = pacman{plHuntingTime = 0.8}

plIsHunting pacman@(Player{plHuntingTime=t}) = t > 0

plMove :: Player -> TimeDelta -> Board -> Player
plMove pacman@(Player pos dir huntingTime) dt board
    = pacman{plPos = defaultMove dt pos dir board plSpeed,
             plHuntingTime = max 0.0 (huntingTime - dt)}

plUpdateDir :: Player -> TimeDelta -> Direction -> Player
plUpdateDir pacman@(Player plPos@(Vector px py) plDir _) dt newDir
    = if plDir == newDir then pacman
      else if (plDir, newDir) `elem` [(N,S), (S,N), (W,E), (E,W)] || newDir == X then pacman {plDir = newDir}
      else if plDir `elem` [E, W] && 15 < (abs $ cx - px) then pacman
      else if plDir `elem` [S, N] && 15 < (abs $ cy - py) then pacman
      else pacman {plDir = newDir, plPos = plPos'}
           where plPos' = centeredPos
                 centeredPos@(Vector cx cy) = getNearestCenter (plPos `vadd` tileHalf) `vsub` tileHalf

---- GameData
data GameData = GameData { balls :: Balls,
                           pacman :: Player,
                           enemies :: Enemies,
                           fruits :: Fruits,
                           board :: Board
                         } deriving (Show)

gdDisplay :: GameData -> ImagesMap -> IO ()
gdDisplay (GameData balls pacman enemies fruits board) imagesMap
    = getVideoSurface >>= (\screen -> displayAt screen >> SDL.flip screen)
      where
        displayAt screen = displayBg >> displayBoard >> displayBalls >> displayFruits >> displayEnemies >> displayPlayer
          where
            displayBg      = mapRGB (surfaceGetPixelFormat screen) 0x1F 0x1F 0x1F >>= (\grey -> fillRect screen Nothing grey)
            displayBoard   = mapM_ (\i_p -> displayBoardPiece imagesMap i_p) $ zip (iterate (+1) 0) board
            displayBalls   = mapM_ (displayImage img_ball) balls
            displayFruits  = mapM_ (displayImage img_fruit) fruits
            displayEnemies = mapM_ ((displayImage img_enemy) . enGetPos) enemies
            displayPlayer  =       ((displayImage (if plIsHunting pacman then img_pacman_hunting else img_pacman)) . plGetPos) pacman
            displayImage imgName (Vector x y) = scBlit screen (surfaceByName imgName) (round x) (round y)
            surfaceByName imgName = fromJust$ lookup imgName imagesMap
            displayBoardPiece imagesMap (i, piece) = mapM_ blit [(piece .&. 1), (piece .&. 2), (piece .&. 4), (piece .&. 8)]
                     where blit p = scBlit screen (surfaceByName (nameOf p)) (tileW * col) (tileH * row)
                           nameOf p = fromJust.lookup p $ fromList [(0, img_board_empty), (1, img_board_top), (2, img_board_right),
                                                                    (4, img_board_bottom), (8, img_board_left)]
                           (row, col) = divMod i boardWidth

gdHandleEvent :: GameData -> TimeDelta -> Event -> IO(GameData)
gdHandleEvent gd _ SDL.NoEvent = return gd
gdHandleEvent gd _ SDL.Quit = exitWith ExitSuccess
gdHandleEvent gd _ (KeyDown (Keysym _ _ 'q')) = exitWith ExitSuccess
gdHandleEvent gd@(GameData _ pacman _ board _) dt (KeyDown (Keysym key _ _))
    = if key `elem` [SDLK_RIGHT, SDLK_LEFT, SDLK_DOWN, SDLK_UP] then return $ gd{pacman = plUpdateDir pacman dt dir}
      else return gd where dir = fromJust.lookup key $ fromList [(SDLK_RIGHT, E), (SDLK_LEFT, W), (SDLK_DOWN, S), (SDLK_UP, N)]
gdHandleEvent gd _ _ = return gd

gdUpdate :: GameData -> TimeDelta -> IO(GameData)
gdUpdate gameData dt
    = do event <- pollEvent
         (GameData balls pacman enemies fruits board) <- gdHandleEvent gameData dt event
         -- balls & fruits
         let (consumedBalls,  balls')  = List.partition (collidesWithPlayer pacman) balls
             (consumedFruits, fruits') = List.partition (collidesWithPlayer pacman) fruits
         if List.null consumedBalls  then return() else putStrLn "Consumed a ball!"
         if List.null consumedFruits then return() else putStrLn "Upgrade!"
         -- pacman
         let pacman'  = if List.null consumedFruits then movedPacman else plMakeHunting movedPacman
                        where movedPacman = plMove pacman dt board
             (collidEnemies, nonCollidEnemies) = List.partition ((collidesWithPlayer pacman).enGetPos) enemies
         -- pacman ate ghost
         if plIsHunting pacman' && notNull collidEnemies then
           do
             putStrLn$ "Consumed " ++ (show$ length collidEnemies) ++ " ghosts!"
             putStrLn$ "Ghosts left: " ++ (show$ length nonCollidEnemies)
           else return()
         -- enemies
         let updateEnemies enemies = map ((if plIsHunting pacman' then enMakeScared else enMakeHunting)
                                          . (\e -> enUpdate e dt pacman board)) enemies
         return $ if notNull collidEnemies && plIsHunting pacman then
                      GameData balls' pacman' (updateEnemies nonCollidEnemies) fruits' board
                  else if notNull collidEnemies && (not$ plIsHunting pacman) then
                      defaultGameData
                  else
                      GameData balls' pacman' (updateEnemies enemies) fruits' board
            where collidesWithPlayer pacman objPos = 10 > vlen (objPos `vsub` plGetPos pacman)

---- Main
loop :: CpuTime -> GameData -> ImagesMap -> IO ()
loop startTime gameData images
    = do endTime <- getCPUTime
         let dt = (fromIntegral (endTime - startTime)) / (10^13)
         gameData' <- gdUpdate gameData dt
         gdDisplay gameData' images
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
       loop startTime defaultGameData images

defaultGameData :: GameData
defaultGameData
    = GameData balls player enemies fruits board
       where enemies = map makeEnemy [(Vector 500 250), (Vector (0*50) (6*50))]
             fruits  = map makeFruit [(Vector (5*50) (3*50)), (Vector (8*50) 0), (Vector (2*50) 0)]
             balls   = map makeBall [ Vector (float x * 50.0) (float y * 50.0) | x <- range (0, 12-1), y <- range(0, 7-1) ]
             player  = makePlayer (Vector 0 0)

             t=1; r=2; b=4; l=8
             tl=t+l; tr=t+r; bl=b+l; br=b+r; lr=l+r; tb=t+b
             board  = [tl,  t, tb, tb,  t, tb, tb, tb, tr, tl,  t, tr,  t,  t,  t, tr,
                       lr, lr, tl, tb,  0, tb,  t, tb,  0, br, lr, lr,  0,  0,  0,  r,
                       lr, bl,  r, tl, br, tl,  0, tr,  l, tr, lr, lr,  0,  0,  0,  r,
                        l, tb,  r, lr, tl, br, lr, lr, bl, br, lr, lr,  0,  0,  0,  r,
                        l, tb, br, lr, bl, tr, lr, lr, tl, tb, br, lr,  0,  0,  0,  r,
                        l, tb, tr, bl, tr, lr, lr, bl,  0,  t, tr, lr,  0,  0,  0,  r,
                       bl, tb,  b, tb,  b, br, bl, tb, br, bl,  b, br,  0,  0,  0,  r,
                       tl,  t,  t,  t,  t,  t,  t,  t,  t,  t,  t,  t,  t,  t,  t, tr,
                        l,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  r,
                        l,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  r,
                        l,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  r,
                       bl,  b,  b,  b,  b,  b,  b,  b,  b,  b,  b,  b,  b,  b,  b, br]
