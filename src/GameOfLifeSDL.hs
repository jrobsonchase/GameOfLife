{-
  Game of Life SDL - Provides an SDL interface for the Game of Life
  Copyright (C) 2012  Josh Chase

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

import GameOfLife
import System.IO
import Data.List
import Data.Word
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Time as SDLTime

data AppConfig = AppConfig {
  screen :: Surface
, screenX :: Int
, screenY :: Int
, gridX :: Int
, gridY :: Int
} deriving (Show)

data World = World {
  cells :: [Cell]
, fpsTimer :: Timer
, frame :: Int
}

gridW env = (screenX env) `div` (gridX env)
gridH env = (screenY env) `div` (gridY env)

type AppState = StateT World IO
type AppEnv = ReaderT AppConfig AppState

type Timer = (Word32, Bool)

clearScreen :: Surface -> IO Bool
clearScreen screen = fillRect screen Nothing (Pixel 0)

promptInt :: String -> IO Int
promptInt s = do
  putStr s
  input <- getLine
  case (reads input :: [(Int,String)]) of
       [(i,_)] -> return i 
       [] -> do putStrLn "Please Enter a Number"
                promptInt s

initEnv :: IO AppConfig
initEnv = do
  let xDim = 50
      yDim = 50
  screen <- setVideoMode 800 800 32 [SWSurface]
  return (AppConfig screen 800 800 xDim yDim)

-- Generate all of the rectangls to be used in the rendering
genRects :: AppEnv [Rect]
genRects = do
  env <- ask
  ps <- get
  let cW = (screenX env) `div` (gridX env)
      cH = (screenY env) `div` (gridY env)
  return . map (\(x,y) -> Rect (x*cW) (y*cH) cW cH) $ (cells ps)

inBounds :: AppConfig -> Cell -> Bool
inBounds env (x,y) = let w = gridW env
                         h = gridH env
                         x' = screenX env
                         y' = screenY env
                     in((x*w) <= x' && (y*h) <= y' && x >= 0 && y >= 0) 

handleEvents :: AppEnv Bool
handleEvents = do
  event <- liftIO pollEvent
  state <- get
  env <- ask
  case event of
       Quit -> return True
       (KeyDown (Keysym key _ _)) -> do
         case key of
              SDLK_SPACE -> put state { cells = (filter (inBounds env) . advanceGrid $ (cells state)) }
              SDLK_s -> liftIO . putStrLn . show $ (cells state)
              _ -> return ()
         return False
       (MouseButtonDown x y ButtonLeft) -> do
         w <- liftM gridW ask
         h <- liftM gridH ask
         let p = ((fromIntegral x) `div` w , (fromIntegral y) `div` h)
         case (alive p (cells state)) of
              True -> put state { cells = (delete p (cells state)) }
              False -> put state { cells = (p:(cells state)) }
         return False
       _ -> return False

limitFPS :: Int -> AppEnv ()
limitFPS fps = do
  time <- liftIO SDLTime.getTicks
  timer <- liftM fpsTimer get
  let delay = ((fromIntegral $ 1000 `div` fps))
  liftIO $ putStrLn ("Delaying..." ++ (show delay))
  liftIO $ SDL.delay delay
  liftIO $ putStrLn "Done Delaying!"

loop :: AppEnv ()
loop = do
  time <- liftIO SDL.getTicks
  modify $ \s -> s { fpsTimer = (time, True) }
  env <- ask
  screen <- liftM screen ask
  current <- get
  quit <- handleEvents
  rects <- genRects
  liftIO . clearScreen $ screen
  liftIO . sequence . map (\r -> fillRect screen (Just r) (Pixel 0x00ff0000)) $ rects
  liftIO . SDL.flip $ screen
  unless quit loop

runLoop :: AppConfig -> World -> IO ()
runLoop env initialState = (evalStateT . runReaderT loop) env initialState

parseFile :: String -> IO [Cell]
parseFile file = do
  contents <- readFile file
  return (foldr (\s acc -> (read s :: [Cell]) ++ acc) [] (lines contents))

main = withInit [InitEverything] $ do
  initialState <- parseFile "points.txt"
  env <- initEnv
  start <- SDLTime.getTicks
  runLoop env (World initialState (start,True) 0)
