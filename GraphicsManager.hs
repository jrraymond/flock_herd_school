module GraphicsManager 
  ( Event (..)
  , Rect (..)
  , KeyState (..)
  , pollEvent
  , updateKeys
  , drawRect
  , drawCircle
  , drawVector ) where

import qualified Graphics.UI.SDL as SDL
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
--import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
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


drawCircle :: SDL.Renderer -> Double -> Double -> Double -> IO CInt
drawCircle r cx cy rad = do
  ps <- newArray $ zipWith (SDL.Point `on` floor) [ cx + rad * cosine a | a <- map Degrees [0..359 :: Double] ] [ cy + rad * sine a | a <- map Degrees [0..359 :: Double] ]
  SDL.renderDrawPoints r ps 360
  --withForeignPtr ps newArray


drawVector :: SDL.Renderer -> CInt -> CInt -> CInt -> CInt -> IO CInt
drawVector r px py vx vy = SDL.renderDrawLine r px py (px + vx) (py + vy)

drawRect :: SDL.Renderer -> Rect -> IO CInt
drawRect r (Rect x y w h) = alloca $ \f -> poke f (SDL.Rect x y w h) >> SDL.renderFillRect r f

data Rect = Rect CInt CInt CInt CInt deriving (Eq, Show)
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
          1 -> liftM interpretEvent $ peek e
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
