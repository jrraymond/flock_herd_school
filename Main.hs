module Main where

import GraphicsManager
import Boids
import Vector2D
import Data.Maybe (listToMaybe)
import System.Exit (exitSuccess)
--import Debug.Trace (trace)
--import qualified Data.Vector as V
import qualified Graphics.UI.SDL as SDL
import System.Environment (getProgName)
import System.Random


main :: IO ()
main = do
    putStrLn "Welcome to Flocks! Enter Q to exit otherwise it enter to continue"
    q <- getLine
    case q of 
              "Q" -> exitSuccess 
              _ -> return ()
    opts <- getGameOpts
    gen <- getStdGen
    let boids = genRandomBoids gen 600 600 (numBoidsOpt opts)
    _ <- SDL.init SDL.initFlagEverything
    n <- getProgName
    w <- getWindow n 600 600 [SDL.windowFlagResizable]
    r <- SDL.createRenderer w (-1) SDL.rendererFlagAccelerated
    _ <- SDL.setRenderDrawColor r 0 0 0 255
    _ <- SDL.renderClear r
    _ <- SDL.renderPresent r
    loop w r (World (Player 300 300 10 10) boids (KeyState False False False False) 600 600)
    SDL.quit
    putStrLn "Game Over\n\n"
    main

loop :: SDL.Window -> SDL.Renderer -> World -> IO ()
loop w r world@(World p bs ks wd ht) = do
    SDL.delay 10
    ne <- pollEvent
    case ne of
        QuitEvent         -> return ()
        ResizeEvent a b c wd' ht' -> putStrLn (show a ++ "\t" ++ show b ++ "\t" ++ show c) >> loop w r (World p bs ks wd' ht')
        MoveEvent typ key -> let bs' = step (fromIntegral wd) (fromIntegral ht) bs [V2 (fromIntegral (xP (playerW w'))) (fromIntegral (yP (playerW w')))]
                                 w' = move (World p bs' (updateKeys ks typ key) wd ht)
                             in render r w' >> loop w r w'
        ContinueEvent     -> let bs' = step (fromIntegral wd) (fromIntegral ht) bs [V2 (fromIntegral (xP (playerW world))) (fromIntegral (yP (playerW world)))]
                                 w' = World p bs' ks wd ht
                             in render r w' >> loop w r w'


data World = World { playerW :: !Player
                   , boidsW :: ![Boid]
                   , keysW :: !KeyState
                   , widthW :: !Int
                   , heightW :: !Int
                   } deriving Show
data Player = Player { xP :: !Int
                     , yP :: !Int
                     , wP :: !Int
                     , hP :: !Int
                     } deriving (Eq, Show)


drawBoid :: SDL.Renderer -> Boid -> IO ()
drawBoid r (Boid _ (V2 x y) _ _) = drawRect r (Rect (floor x) (floor y) 5 5)

drawBoid' :: SDL.Renderer -> Boid -> IO ()
drawBoid' r (Boid _ (V2 px py) (V2 vx vy) _) = do
    let px' = floor px
        py' = floor py
        vx' = floor vx
        vy' = floor vy
    _ <- drawVector r px' py' vx' vy'
    _ <- drawCircle r px py nearC
    drawRect r (Rect px' py' 5 5)

move :: World -> World 
move (World (Player x y wd ht) bs ks@(KeyState r l d u) wW hW) = World p' bs ks wW hW where
  p' = Player (x' `mod` wW) (y' `mod` hW) wd ht
  x' | r = x + 5 | l = x - 5 | otherwise = x
  y' | d = y + 5 | u = y - 5 | otherwise = y

render :: SDL.Renderer -> World -> IO ()
render r (World (Player x y w h) bs _ _ _) = do
    _ <- SDL.setRenderDrawColor r 0 0 0 255
    _ <- SDL.renderClear r
    _ <- SDL.setRenderDrawColor r 255 255 255 255 
    _ <- drawRect r (Rect x y w h)
    _ <- SDL.setRenderDrawColor r 0 255 0 255 
    _ <- mapM (drawBoid r) bs
    SDL.renderPresent r


getGameOpts :: IO Opts
getGameOpts = do
    putStrLn "Enter name:"
    n <- getLine
    putStrLn "Enable mouse? (T/F): "
    m <- getLine
    let m' = head (m ++ " ") == 'T' -- hack to avoid partiality
    putStrLn "Number of boids? (1-99): "
    b <- getLine
    let b' = case maybeReads b :: Maybe Int of
               Nothing -> 10
               Just i | i < 1 -> 1 
                      | i > 99 -> 99
                      | otherwise -> i
    return $ Opts n m' b'

genRandomBoids :: StdGen -> Int -> Int -> Int -> [Boid]
genRandomBoids gen w h = f gen where
  f _ 0 = []
  f g i = let (px,g') = randomR (0,fromIntegral w) g
              (py,g'') = randomR (0,fromIntegral h) g'
              (vx,g''') = randomR (-2,2) g''
              (vy,g'''') = randomR (-2,2) g'''
          in Boid (i - 1) (V2 px py) (V2 vx vy) (V2 0 0) : f g'''' (i - 1)


maybeReads :: Read a => String -> Maybe a
maybeReads = fmap fst . listToMaybe . reads


data Opts = Opts { nameOpt :: !String
                 , mouseOpt :: !Bool -- true to enable mouse, false to use arrow keys
                 , numBoidsOpt :: !Int -- number of boids
                 } deriving (Eq, Show)
