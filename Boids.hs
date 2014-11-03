module Boids where

--import Debug.Trace (trace)
import Data.List (foldl', delete)
import Vector2D

data Boid = Boid { _idBd :: !Int
                 , _posBd :: !V2
                 , _velBd :: !V2 } deriving (Show, Eq)
  --               , _maxV :: !Double
  --               , _aBd :: !Double
  --               , _sBd :: !Double
  --               , _cBd :: !Double } deriving (Show, Eq)
sepC :: Double
sepC = 0.5
alignC :: Double
alignC = 1
cohC :: Double
cohC = 1
nearC :: Double
nearC = 20
velMax :: Double
velMax = 10
avdC :: Double
avdC = 0.5

step :: Double -> Double -> [Boid] -> [V2] -> [Boid]
step w h bs ps = map (stepBoid w h bs ps nearC) bs

stepBoid :: Double -> Double -> [Boid] -> [V2] -> Double -> Boid -> Boid
stepBoid w h bs ps d b@(Boid i pos vel) = b' where
  -- | trace ("pos "++show pos++" vel "++show vel++" sV "++show sV++" aV"++show aV++" cV"++show cV++" v'"++show v'++" near "++show nears) False = undefined
  -- | otherwise = b' where
  bs' = delete b bs
  nears = filter (\x -> mag2 (sub pos (_posBd x)) < d ** 2) bs'
  sV = seperate (map (sub pos . _posBd) nears)
  aV = align (map _velBd nears)
  cV = coher (map _posBd nears) pos
  xV = avoid (map (sub pos) (filter (\x -> mag2 (sub pos x) < d ** 2) ps))
  v' = clamp velMax $ add5 sV aV cV vel xV
  b' = Boid i (wrap w h $ add pos v') v'

{- Avoid collisions with neighboring boids position diffs -} 
seperate :: [V2] -> V2
seperate = scale sepC . foldl' add (V2 0 0)

{- Match the velocity of neighboring boids -}
align :: [V2] -> V2
align vs = 
    case vs of
      [] -> V2 0 0
      _ -> scale (alignC / fromIntegral (length vs)) $ foldl' add (V2 0 0) vs

{- Stay close to neighbooring boids -}
coher :: [V2] -> V2 -> V2
coher ps pos = 
    case ps of
      [] -> V2 0 0 
      _ -> scale cohC $ sub (getCenter ps) pos

{- Avoid Predators -}
avoid :: [V2] -> V2
avoid = scale avdC . foldl' add (V2 0 0)

getCenter :: [V2] -> V2
getCenter ps = scale (1 / fromIntegral (length ps)) $ foldl' add (V2 0 0) ps
