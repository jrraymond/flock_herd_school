module Main where

import GraphicsManager
import Boids
import Vector2D
--import Debug.Trace (trace)
--import qualified Data.Vector as V
import qualified Graphics.UI.SDL as SDL
import System.Environment (getProgName)


main :: IO ()
main = do 
    _ <- SDL.init SDL.initFlagEverything
    n <- getProgName
    w <- getWindow n wWd wHt
    r <- SDL.createRenderer w (-1) SDL.rendererFlagAccelerated
    print r
    _ <- SDL.renderSetLogicalSize r (fromIntegral wWd) (fromIntegral wHt)
    _ <- SDL.setRenderDrawColor r 0 0 0 255
    _ <- SDL.renderClear r
    _ <- SDL.renderPresent r
    loop w r (World (Player (wWd `div` 2) (wHt `div` 2) 10 10) boids (KeyState False False False False))
    SDL.quit
    putStrLn "Done"

loop :: SDL.Window -> SDL.Renderer -> World -> IO ()
loop w r world@(World p bs ks) = do
    SDL.delay 50
    SDL.flushEvents SDL.scancodeRight SDL.scancodeUp
    ne <- pollEvent
    case ne of
        Quit           -> return ()
        Move typ key   -> let bs' = step (fromIntegral wWd) (fromIntegral wHt) bs [V2 (fromIntegral (xP (playerW w'))) (fromIntegral (yP (playerW w')))]
                              w' = move (World p bs' $ updateKeys ks typ key) 
                          in render r w' >> loop w r w'
        Continue       -> let bs' = step (fromIntegral wWd) (fromIntegral wHt) bs [V2 (fromIntegral (xP (playerW world))) (fromIntegral (yP (playerW world)))]
                              w' = World p bs' ks
                          in render r w' >> loop w r w'



wWd :: Int
wWd = 800
wHt :: Int
wHt = 600

data World = World { playerW :: !Player
                   , boidsW :: ![Boid]
                   , keysW :: !KeyState
                   } deriving Show
data Player = Player { xP :: !Int
                     , yP :: !Int
                     , wP :: !Int
                     , hP :: !Int
                     } deriving (Eq, Show)


drawBoid :: SDL.Renderer -> Boid -> IO ()
drawBoid r (Boid _ (V2 x y) _) = drawRect r (Rect (floor x) (floor y) 5 5)

drawBoid' :: SDL.Renderer -> Boid -> IO ()
drawBoid' r (Boid _ (V2 px py) (V2 vx vy)) = do
    let px' = floor px
        py' = floor py
        vx' = floor vx
        vy' = floor vy
    _ <- drawVector r px' py' vx' vy'
    _ <- drawCircle r px py nearC
    drawRect r (Rect px' py' 5 5)

move :: World -> World 
move (World (Player x y wd ht) bs ks@(KeyState r l d u)) = World p' bs ks where
  p' = Player (x' `mod` wWd) (y' `mod` wHt) wd ht
  x' | r = x + 5 | l = x - 5 | otherwise = x
  y' | d = y + 5 | u = y - 5 | otherwise = y

render :: SDL.Renderer -> World -> IO ()
render r (World (Player x y w h) bs _) = do
    _ <- SDL.setRenderDrawColor r 0 0 0 255
    _ <- SDL.renderClear r
    _ <- SDL.setRenderDrawColor r 255 255 255 255 
    _ <- drawRect r (Rect x y w h)
    _ <- SDL.setRenderDrawColor r 0 255 0 255 
    _ <- mapM (drawBoid r) bs
    SDL.renderPresent r




boids :: [Boid]
boids = [ Boid 0  (V2 400 500) (V2   1    0.7 )
        , Boid 1  (V2 301 401) (V2   1    1 )
        , Boid 2  (V2 202 302) (V2   1    0 )
        , Boid 3  (V2 503 203) (V2   1  (-1))
        , Boid 4  (V2 604 104) (V2   0    1 )
        , Boid 5  (V2 305 505) (V2   0    0 )
        , Boid 6  (V2 206 406) (V2   0    1.5 )
        , Boid 7  (V2 507 307) (V2   2    0 )
        , Boid 8  (V2 608 208) (V2   0    2 )
        , Boid 9  (V2 409 109) (V2   0    0.4 )
        , Boid 10 (V2 310 510) (V2   2    2 )
        , Boid 11 (V2 411 411) (V2   2    0 )
        , Boid 12 (V2 212 312) (V2   1    2.3 )
        , Boid 13 (V2 413 213) (V2   1    0 )
        , Boid 14 (V2 514 114) (V2   2    1 )
        , Boid 15 (V2 415 315) (V2   2    0.1 )
        , Boid 16 (V2 616 216) (V2   2    1 )
        , Boid 17 (V2 117 517) (V2 (-1)   0 )
        , Boid 18 (V2 718 318) (V2 (-1) (-1))
        , Boid 19 (V2 419 119) (V2 (-3)   1 )
        , Boid 20 (V2 010 000) (V2   1    0.7 )
        , Boid 21 (V2 029 401) (V2   1    1 )
        , Boid 22 (V2 132 302) (V2   1    0 )
        , Boid 23 (V2 043 203) (V2   1  (-1))
        , Boid 24 (V2 154 104) (V2   0    1 )
        , Boid 25 (V2 065 505) (V2   0    0 )
        , Boid 26 (V2 176 406) (V2   0    1.5 )
        , Boid 27 (V2 087 307) (V2   2    0 )
        , Boid 28 (V2 198 208) (V2   0    2 )
        , Boid 29 (V2 409 109) (V2   0    0.4 )
        , Boid 30 (V2 310 510) (V2   2    2 )
        , Boid 31 (V2 511 171) (V2   2    0 )
        , Boid 32 (V2 612 192) (V2   1    2.3 )
        , Boid 33 (V2 713 013) (V2   1    0 )
        , Boid 34 (V2 314 114) (V2   2    1 )
        , Boid 35 (V2 415 015) (V2   2    0.1 )
        , Boid 36 (V2 516 216) (V2   2    1 )
        , Boid 37 (V2 317 017) (V2 (-1)   0 )
        , Boid 38 (V2 218 518) (V2 (-1) (-1))
        , Boid 39 (V2 119 019) (V2 (-3)   1 )
        ]

