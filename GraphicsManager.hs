module GraphicsManager where

import qualified Graphics.UI.SDL as SDL

type Width = Int
type Height = Int
data Rect = Rect { _xR :: !Int, _yR :: !Int, _wR :: !Int, _hR :: !Int } deriving (Eq, Show)

--type Color = (Int,Int,Int)
--
--drawRect :: SDL.Surface -> Rect -> Color -> IO ()
--drawRect screen (Rect x y w h) (r,g,b) = do
--    color <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen (fromIntegral r) (fromIntegral g) (fromIntegral b)
--    SDL.fillRect screen (Just (SDL.Rect x y w h)) color
--    return ()
