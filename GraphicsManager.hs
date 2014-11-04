module GraphicsManager 
  ( Event (..)
  , Rect (..)
  , KeyState (..)
  , pollEvent
  , updateKeys
  , drawRect
  , drawCircle
  , drawVector
  , getWindow
  , drawStartMenu
  ) where

import qualified Graphics.UI.SDL as SDL
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.Storable
import Foreign.C.String
import Control.Monad
import Data.Word
import Data.Angle
import Data.Function (on)


data KeyState = KeyState Bool Bool Bool Bool deriving (Eq, Show)
updateKeys :: KeyState -> Word32 -> SDL.Scancode -> KeyState
updateKeys ks@(KeyState r l d u) 768 key
  | key == SDL.scancodeRight = KeyState True l d u
  | key == SDL.scancodeLeft = KeyState r True d u
  | key == SDL.scancodeDown = KeyState r l True u
  | key == SDL.scancodeUp = KeyState r l d True
  | otherwise = ks
updateKeys ks@(KeyState r l d u) 769 key
  | key == SDL.scancodeRight = KeyState False l d u
  | key == SDL.scancodeLeft = KeyState r False d u
  | key == SDL.scancodeDown = KeyState r l False u
  | key == SDL.scancodeUp = KeyState r l d False
  | otherwise = ks
updateKeys ks _ _ = ks  

toCInt :: Int -> CInt
toCInt = fromIntegral

getWindow :: String -> Int -> Int -> IO SDL.Window
getWindow s w h = do
    s' <- newCString s
    SDL.createWindow s' SDL.windowPosUndefined SDL.windowPosUndefined (toCInt w) (toCInt h) SDL.windowFlagResizable

drawStartMenu :: SDL.Renderer -> String -> Int -> Int -> IO ()
drawStartMenu r s w h = do
    _ <- SDL.setRenderDrawColor r 255 255 255 255
    drawRect r (Rect 0 0 w h)
    _ <- SDL.renderPresent r
    SDL.delay 10000

drawCircle :: SDL.Renderer -> Double -> Double -> Double -> IO ()
drawCircle r cx cy rad = do
  ps <- newArray $ zipWith (SDL.Point `on` floor) [ cx + rad * cosine a | a <- map Degrees [0..359 :: Double] ] [ cy + rad * sine a | a <- map Degrees [0..359 :: Double] ]
  _ <- SDL.renderDrawPoints r ps 360
  return ()


drawVector :: SDL.Renderer -> Int -> Int -> Int -> Int -> IO ()
drawVector r px py vx vy = 
    let p1x = toCInt px
        p1y = toCInt py
        p2x = p1x + toCInt vx
        p2y = p1y + toCInt vy
    in SDL.renderDrawLine r p1x p1y p2x p2y >> return ()

drawRect :: SDL.Renderer -> Rect -> IO ()
drawRect r (Rect x y w h) = 
    let rect' = SDL.Rect (toCInt x) (toCInt y) (toCInt w) (toCInt h)
    in alloca $ \f -> poke f rect' >> SDL.renderFillRect r f >> return ()

data Rect = Rect !Int !Int !Int !Int deriving (Eq, Show)
data Event = Quit | Continue | Move Word32 SDL.Scancode deriving (Eq, Show)


{- alloca :: Storable a => (Ptr a -> IO b) -> IO b
- alloca f executes the computation f, passing as argument a pointer to
- a temporarily allocated block of memory sufficient to hold values of type
- a.
- The memory is freed when f terminates (either normally or via an
- exception), so the pointer passed to f must not be used after this.
-
- peek :: Ptr a -> IO a ; reads a value from a memory location
-}
pollEvent :: IO Event
pollEvent = alloca poll where
    poll e = do
      r <- SDL.pollEvent e
      case r of 
          1 -> SDL.flushEvent SDL.eventTypeKeyDown >> liftM interpretEvent (peek e)
          _ -> return Continue

interpretEvent :: SDL.Event -> Event
interpretEvent e = 
    case e of
       SDL.QuitEvent _ _                                   -> Quit
       SDL.KeyboardEvent typ _ _ _ _ (SDL.Keysym code _ _) -> Move typ code
       _                                                   -> Continue
--KeyboardEvent {eventType = 768
--              , eventTimestamp = 16677
--              , keyboardEventWindowID = 2
--              , keyboardEventState = 1
--              , keyboardEventRepeat = 0
--              , keyboardEventKeysym = Keysym {keysymScancode = 44
                              --              , keysymKeycode = 32
                              --              , keysymMod = 0}}
