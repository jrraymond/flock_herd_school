module Boids where

--import Debug.Trace (trace)
import Data.List (foldl', delete)
import Vector2D

data Boid = Boid { _idBd :: !Int
                 , _posBd :: !V2
                 , _velBd :: !V2
                 , _aceBd :: !V2 } deriving (Show, Eq)

sepC :: Double
sepC = 1
alignC :: Double
alignC = 1
cohC :: Double
cohC = 1
nearC :: Double
nearC = 50
velMax :: Double
velMax = 1
acelMax :: Double
acelMax = 0.01
avdC :: Double
avdC = 1
avdDst :: Double
avdDst = 2 * nearC

{- width height boids predators obstacles goals -}
step :: Double -> Double -> [Boid] -> [V2] -> [Obj] -> [Obj] -> [Boid]
step w h bs ps os gs = map (stepBoid w h bs ps os gs nearC) bs

stepBoid :: Double -> Double -> [Boid] -> [V2] -> [Obj] -> [Obj] -> Double -> Boid -> Boid
stepBoid w h bs ps os gs d b@(Boid i pos vel acel) = b' where
  bs' = delete b bs
  nears = filter (\x -> mag2 (sub pos (_posBd x)) < d ** 2) bs'
  sV = scale sepC $ seperate bs' pos (d ** 2 / 4)
  aV = align (map _velBd nears)
  cV = coher (map _posBd nears) pos
  xV = avoid (map (sub pos) (filter (\x -> mag2 (sub pos x) < avdDst ** 2) ps))
  a' = limit acelMax $ add5 sV aV cV acel xV
  v' = limit velMax $ add vel a'
  b' = Boid i (wrap w h $ add pos v') v' a'

{- Avoid collisions with neighboring boids position diffs -} 
seperate :: [Boid] -> V2 -> Double -> V2
seperate ps p r2 = 
    case ps of
      [] -> V2 0 0 
      _ -> let (v,a) = foldl' f (V2 0 0,0) ps in if a == 0 then v else scale (1 / a) v
  where
      f (v,a) b = let vd = sub p (_posBd b)  --calculate vector between boids
                      d = mag2 vd            --calculate distance squared
                  in if d < r2 && d > 0       -- if the boid is close and avoid division by 0
                       then (add v $  scale (1 / d) (normalize vd), a + 1)   -- scale seperation vector by inverse of distance squared
                       else (v,a)   -- if outside the range it does not contribute to seperation


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
      _ -> scale cohC $ sub (getCenter (pos:ps)) pos

{- Avoid Predators -}
avoid :: [V2] -> V2
avoid ps = 
    case ps of
      [] -> V2 0 0
      _ -> scale (avdC / fromIntegral (length ps)) $ foldl' add (V2 0 0) ps

getCenter :: [V2] -> V2
getCenter ps = scale (1 / fromIntegral (length ps)) $ foldl' add (V2 0 0) ps

data Obj = Sphere V2 Double deriving (Show, Eq)
