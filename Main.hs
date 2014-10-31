module Main where

--import GraphicsManager
import Physics

--import qualified Data.Vector as V
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.OpenGL as GL
import System.Environment (getProgName)
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import Control.Monad
import Data.Bits

wWd :: Foreign.C.Types.CInt
wWd = 800
wHt :: Foreign.C.Types.CInt
wHt = 600

main :: IO ()
main = do 
    _ <- SDL.init SDL.initFlagEverything
    n <- getProgName
    w <- withCString n getWindow
    _ <- SDL.glCreateContext w
--    r <- SDL.createRenderer w (-1) SDL.rendererFlagAccelerated
--    _ <- SDL.setRenderDrawColor r 0 0 0 1
--    _ <- SDL.renderClear r
    loop w
    SDL.quit
    putStrLn "Done"

loop :: SDL.Window -> IO ()
loop w = do
    drawGL w
    ne <- Main.pollEvent
    case ne of
        Quit -> return ()
        Continue -> loop w

data Event = Quit | Continue deriving (Eq, Show)


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
interpretEvent e = 
    case e of
       SDL.QuitEvent _ _ -> Quit
       _ -> Continue

getWindow :: CString -> IO SDL.Window
getWindow s = do
    w <- SDL.createWindow s SDL.windowPosUndefined SDL.windowPosUndefined  wWd wHt $ SDL.windowFlagResizable .|. SDL.windowFlagOpenGL
    GL.depthFunc GL.$= Just GL.Less
    GL.clearColor GL.$= GL.Color4 0 0 0 1
    GL.viewport GL.$= (GL.Position 0 0, GL.Size 800 600)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GL.perspective 45 (800 / 600) 0.1 100
    GL.matrixMode GL.$= GL.Modelview 0 
    return w

drawGL :: SDL.Window -> IO ()
drawGL w = do
    GL.clear [GL.ColorBuffer,GL.DepthBuffer]
    GL.loadIdentity
    GL.scale 0.5 0.5 (0.5 :: GL.GLfloat)
    GL.color $ GL.Color3 1 0 (0 :: GL.GLfloat)
    drawSquare 0.5
    GL.rotate (45 :: GL.GLfloat) (GL.Vector3 1 0 0)
    GL.color $ GL.Color3 0 1 (0 :: GL.GLfloat)
    drawSquare 0.5
    GL.rotate (45 :: GL.GLfloat) (GL.Vector3 0 1 0)
    GL.color $ GL.Color3 0 0 (1 :: GL.GLfloat)
    drawSquare 0.5
  --  drawCube 0.3
    SDL.glSwapWindow w

drawSquare :: GL.GLfloat -> IO ()
drawSquare l = GL.renderPrimitive GL.Quads $ mapM_ vertex3f
  [ (l,l,l), (l,l,-l), (l, -l, -l), (l, -l, l) ]

drawCube :: GL.GLfloat -> IO ()
drawCube w = GL.renderPrimitive GL.Quads $ mapM_ vertex3f
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

vertex3f :: GL.VertexComponent a => (a, a, a) -> IO ()
vertex3f (x, y ,z) = GL.vertex $ GL.Vertex3 x y z

data Boid = Boid { _idBd :: !Int
                 , _posBd :: !V2
                 , _velBd :: !V2 }
