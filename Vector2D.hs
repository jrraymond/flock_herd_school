module Vector2D 
 ( V2 (..)
 , add
 , add3
 , add4
 , add5
 , sub
 , neg
 , scale
 , normalize
 , dot
 , mag2
 , mag
 , wrap
 , clamp
 , limit
 ) where

import Control.Monad (join)

data V2 = V2 !Double !Double deriving (Eq,Show)

add :: V2 -> V2 -> V2
add = v2app (+)

add3 :: V2 -> V2 -> V2 -> V2
add3 x y z = v2app (+) x $ v2app(+) y z

add4 :: V2 -> V2 -> V2 -> V2 -> V2
add4 w x y z = v2app (+) w $ v2app (+) x $ v2app (+) y z

add5 :: V2 -> V2 -> V2 -> V2 -> V2 -> V2
add5 v w x y z = v2app (+) v $ v2app (+) w $ v2app (+) x $ v2app (+) y z

sub :: V2 -> V2 -> V2 
sub = v2app (-)

neg :: V2 -> V2
neg = v2map (* (-1))

scale :: Double -> V2 -> V2
scale s = v2map (* s)

normalize :: V2 -> V2
normalize v = v2map (/ mag v) v

dot :: V2 -> V2 -> Double
dot (V2 x1 y1) (V2 x2 y2) = x1 * x2 + y1 * y2

mag2 :: V2 -> Double
mag2 = join dot

mag :: V2 -> Double
mag = sqrt . mag2

wrap :: Double -> Double -> V2 -> V2
wrap w h (V2 x y) = let x' | x > w = 0 | x < 0 = w | otherwise = x
                        y' | y > h = 0 | y < 0 = h | otherwise = y
                    in V2 x' y'

limit :: Double -> V2 -> V2
limit l v
  | mag2 v > l ** 2 = scale l $ normalize v
  | otherwise = v

clamp :: Double -> Double -> Double -> Double -> V2 -> V2
clamp lx ux ly uy (V2 x y) = 
    let x' | x < lx = lx | x > ux = ux | otherwise = x
        y' | y < ly = ly | y > uy = uy | otherwise = y
    in V2 x' y'

v2map :: (Double -> Double) -> V2 -> V2
v2map f (V2 x y) = V2 (f x) (f y)

v2app :: (Double -> Double -> Double) -> V2 -> V2 -> V2
v2app f (V2 x1 y1) (V2 x2 y2) = V2 (f x1 x2) (f y1 y2)
