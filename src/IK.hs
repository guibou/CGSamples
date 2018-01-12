module IK where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data Model = Model Point [Bone] deriving (Show)
data Bone = Bone Float deriving (Show)

data FKData = FKData Float deriving (Show)

displayChainOfDots joint = line joint : map (dotAt red 3) joint

toPic (Model start bones) angles = Pictures (displayChainOfDots joints)
  where
    joints = map fst $ scanl nextJoint (start, 0) (zip angles bones)

    nextJoint (currentP, currentAngle) (FKData a, Bone l) = (nextP, nextAngle)
      where
        nextAngle = currentAngle + a
        nextP = addLengthWithAngle currentP l nextAngle

addLengthWithAngle (x, y) l a = (x + l * cos a, y + l * sin a)

dotAt c s (x, y) = translate x y (color c (circleSolid s))

model = Model (0, 0) (map Bone [100, 50, 10])

fkdata t = map FKData [t, pi / 2, -pi / 2]

run = animate (InWindow "Nice Window" (200, 200) (10, 10)) white (\t -> toPic model (fkdata t))

modelIK = Model (0, 0) (map Bone (replicate 7 20))

getChain (Model start bones) = scanl fStart start bones
  where
    fStart (cx, cy) (Bone size) = (cx - size, cy)

solveIK :: Model -> Point -> [Point]
solveIK model@(Model start bones) target = solvedChain
  where
    startingPoints = getChain model

    sizes = map (\(Bone s) -> s) bones

    solvedChain = (iterate (completeStep sizes start target) startingPoints) !! 1000

completeStep size startTarget endTarget pts = let
  backwardStep = aStep (zip (reverse pts) (0:reverse size)) endTarget
  forwardStep = aStep (zip (reverse backwardStep) (0:size)) startTarget
  in forwardStep

aStep [] _ = []
aStep ((currentPoint, distanceToTarget):xs) target = newPoint : aStep xs newPoint
  where newPoint = (distanceToTarget `vecMul` v) `vecAdd` target
        v = target --> currentPoint

vecAdd (a, b) (da, db) = (a + da, b + db)
vecMul f (x, y) = (f * x, f * y)

(x, y) --> (x', y')
 | norm == 0 = (-1, 0)
 | otherwise = (dx / norm, dy / norm)
  where
    dx = x' - x
    dy = y' - y

    norm = sqrt $ dx * dx + dy * dy

runIK = play (InWindow "Nice Window" (200, 200) (10, 10)) white 24 (0, (0, 0)) (toPicIK modelIK) inputF stepWorld

toPicIK model (t, target) = Pictures $ (dotAt blue 5 startingPoint) : (dotAt green 5 target) : displayChainOfDots (solveIK model target)
  where Model startingPoint _ = model

inputF (EventMotion p) (t, _) = (t, p)
inputF _ w = w

stepWorld dt (t, p) = (t + dt, p)
