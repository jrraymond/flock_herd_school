module Physics where

data V2 = V2 !Double !Double deriving (Eq,Show)

v2map :: (Double -> Double) -> V2 -> V2
v2map f (V2 x y) = V2 (f x) (f y)

v2app :: (Double -> Double -> Double) -> V2 -> V2 -> V2
v2app f (V2 x1 y1) (V2 x2 y2) = V2 (f x1 x2) (f y1 y2)


