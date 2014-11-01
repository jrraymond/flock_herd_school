module Main where

--import GraphicsManager
import Physics
import Debug.Trace (trace)
--import qualified Data.Vector as V
import qualified Graphics.UI.SDL as SDL
import System.Environment (getProgName)
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import Control.Monad

wWd :: Foreign.C.Types.CInt
wWd = 800
wHt :: Foreign.C.Types.CInt
wHt = 600

main :: IO ()
main = do 
    _ <- SDL.init SDL.initFlagEverything
    n <- getProgName
    w <- withCString n getWindow
    r <- SDL.createRenderer w (-1) SDL.rendererFlagAccelerated
    print r
    _ <- SDL.renderSetLogicalSize r 800 600
    _ <- SDL.setRenderDrawColor r 0 0 0 255
    _ <- SDL.renderClear r
    _ <- SDL.renderPresent r
    loop w r (Player 400 300 50 50)
    SDL.quit
    putStrLn "Done"

loop :: SDL.Window -> SDL.Renderer -> Player -> IO ()
loop w r p = do
    ne <- Main.pollEvent
    case ne of
        Quit           -> return ()
        Move key -> let p' = move p key in render r p' >> loop w r p'
        Continue       -> loop w r p

move :: Player -> SDL.Scancode -> Player
move p@(Player x y w h) key
  | key == SDL.scancodeRight = Player (x + 1) y w h
  | key == SDL.scancodeLeft  = Player (x - 1) y w h
  | key == SDL.scancodeUp    = Player x (y - 1) w h
  | key == SDL.scancodeDown  = Player x (y + 1) w h
  | otherwise = p

render :: SDL.Renderer -> Player -> IO ()
render r (Player x y w h) = do
    _ <- SDL.setRenderDrawColor r 0 0 0 255
    _ <- SDL.renderClear r
    _ <- SDL.setRenderDrawColor r 255 255 255 255 
    _ <- drawRect r (Rect x y w h)
    SDL.renderPresent r

drawRect :: SDL.Renderer -> Rect -> IO CInt
drawRect r (Rect x y w h) = alloca $ \f -> poke f (SDL.Rect x y w h) >> SDL.renderFillRect r f

--drawPlayer :: SDL.Renderer -> Player -> Event -> IO ()
--drawPlayer r (Player x y w h) m = SDL.renderFillRect r (SDL.Rect x y w h)  >> SDL.renderPresent r
data Rect = Rect CInt CInt CInt CInt deriving (Eq, Show)
data Player = Player CInt CInt CInt CInt deriving (Eq, Show)
data Event = Quit | Continue | Move SDL.Scancode deriving (Eq, Show)


{- alloca :: Storable a => (Ptr a -> IO b) -> IO b
- alloca f executes the computation f, passing as argument a pointer to
- a temporarily allocated block of memory sufficient to hold values of type
- a.
- The memory is freed when f terminates (either normally or via an
- exception), so the pointer passed to f must not be used after this.
-
- peek :: Ptr a -> IO a ; reads a value from a memory location
-}
pollEvent :: IO Main.Event
pollEvent = alloca poll where
    poll e = do
      r <- SDL.pollEvent e
      case r of 
          1 -> liftM interpretEvent $ peek e
          _ -> return Continue

interpretEvent :: SDL.Event -> Main.Event
interpretEvent e | trace (show e) False = undefined | otherwise = 
    case e of
       SDL.QuitEvent _ _                                 -> Quit
       SDL.KeyboardEvent _ _ _ _ _ (SDL.Keysym code _ _) -> Move code
       _                                                 -> Continue
--KeyboardEvent {eventType = 768
--              , eventTimestamp = 16677
--              , keyboardEventWindowID = 2
--              , keyboardEventState = 1
--              , keyboardEventRepeat = 0
--              , keyboardEventKeysym = Keysym {keysymScancode = 44
                              --              , keysymKeycode = 32
                              --              , keysymMod = 0}}
getWindow :: CString -> IO SDL.Window
getWindow s = SDL.createWindow s SDL.windowPosUndefined SDL.windowPosUndefined  wWd wHt SDL.windowFlagResizable

data Boid = Boid { _idBd :: !Int
                 , _posBd :: !V2
                 , _velBd :: !V2 }
