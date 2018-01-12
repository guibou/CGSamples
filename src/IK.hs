module IK where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data Model = Model Point [Bone] deriving (Show)
data Bone = Bone Float deriving (Show)

--IK

modelIK :: Model
modelIK = Model (0, 0) (map Bone (replicate 7 20))

getInitChain :: Model -> [Point]
getInitChain (Model start bones) = scanl fStart start bones
  where
    fStart (cx, cy) (Bone size) = (cx - size, cy)

solveIK :: Model -> Point -> [Point]
solveIK model@(Model start bones) target = solvedChain
  where
    startingPoints = getInitChain model

    sizes = map (\(Bone s) -> s) bones

    solvedChain = (iterate (completeStep sizes start target) startingPoints) !! 1000

completeStep :: [Float] -> Point -> Point -> [Point] -> [Point]
completeStep size startTarget endTarget pts = let
  backwardStep = aStep (zip (reverse pts) (0:reverse size)) endTarget
  forwardStep = aStep (zip (reverse backwardStep) (0:size)) startTarget
  in forwardStep

aStep :: [(Point, Float)] -> Point -> [Point]
aStep [] _ = []
aStep ((currentPoint, distanceToTarget):xs) target = newPoint : aStep xs newPoint
  where newPoint = (distanceToTarget `vecMul` v) `vecAdd` target
        v = target --> currentPoint

runIK :: IO ()
runIK = play (InWindow "Nice Window" (200, 200) (10, 10)) white 24 (0, (0, 0)) (toPicIK modelIK) inputF stepWorld

toPicIK :: Model -> (Float, Point) -> Picture
toPicIK model (_, target) = Pictures $ (dotAt blue 5 startingPoint) : (dotAt green 5 target) : displayChainOfDots (solveIK model target)
  where Model startingPoint _ = model

inputF :: Event -> (Float, Point) -> (Float, Point)
inputF (EventMotion p) (t, _) = (t, p)
inputF _ w = w

stepWorld :: Float -> (Float, Point) -> (Float, Point)
stepWorld dt (t, p) = (t + dt, p)

-- display utils
dotAt :: Color -> Float -> Point -> Picture
dotAt c s (x, y) = translate x y (color c (circleSolid s))

displayChainOfDots :: [Point] -> [Picture]
displayChainOfDots joint = line joint : map (dotAt red 3) joint

-- Vector utils (on tuple of points)
vecAdd :: Point -> Point -> Point
vecAdd (a, b) (da, db) = (a + da, b + db)

vecMul :: Float -> Point -> Point
vecMul f (x, y) = (f * x, f * y)

(-->) :: Point -> Point -> Point
(x, y) --> (x', y')
 | norm == 0 = (-1, 0)
 | otherwise = (dx / norm, dy / norm)
  where
    dx = x' - x
    dy = y' - y

    norm = sqrt $ dx * dx + dy * dy
