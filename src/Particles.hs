{-# LANGUAGE MultiWayIf #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import qualified Data.HashMap.Strict as HashMap
import qualified System.Random as Random

import Data.Maybe (fromMaybe)
import Control.Monad (guard, replicateM)
import Data.Foldable (foldl')

import Debug.Trace

data V2 = V2 !Float !Float
  deriving (Show)

data Particle = Particle { position :: !V2
                         , oldPosition :: !V2
                         , velocity :: !V2
                         }
  deriving (Show)
w,h :: Int
w = 400
h = 600

run :: IO ()
run = do
  part <- makeScene nbParticules

  play (InWindow "Nice Window" (w, h) (10, 10)) white 24 part displayModel handleInput stepWorld

makeScene :: Int -> IO [Particle]
makeScene count = do
  posX <- replicateM count Random.randomIO
  posY <- replicateM count Random.randomIO

  pure (zipWith makeParticle posX posY)

makeParticle x y = Particle p p (V2 0 0)
  where
    p = V2 (0.2 + x * 0.6) (0.2 + y * 0.8)

displayModel :: [Particle] -> Picture
displayModel ps = translate (-fromIntegral (w `div` 2)) (-fromIntegral (h `div` 2)) $ Pictures (color black (Polygon [(0, 0), (fromIntegral w, 0), (fromIntegral w, fromIntegral h), (0, fromIntegral h)]) : map displayParticule ps)

displayParticule (Particle (V2 x y) _ _) = translate (x * fromIntegral w) (y * fromIntegral h) (color red (circleSolid 2))

handleInput :: Event -> [t] -> [t]
handleInput _e = id

stepWorld :: Float -> [Particle] -> [Particle]
stepWorld dt = (makeParticle 0.5 1 :)
               . map (nextVelocity dt)
               . (densityRelaxation dt radius)
               . map (applySpringDisplacement dt)
               . map (advanceParticule dt)
               . map (applyGravity dt)

g :: V2
g = V2 0 (-0.1)

k, kNear, rho0, radius, outsideForce :: Float
k = 0.1
kNear = 0.5
rho0 = 2
radius = 0.05
outsideForce = 500

nbParticules :: Int
nbParticules = 1000

applyGravity :: Float -> Particle -> Particle
applyGravity dt p = p { velocity = velocity p .+. dt *. g }

advanceParticule :: Float -> Particle -> Particle
advanceParticule dt p = p {oldPosition = position p,
                           position = position p .+. dt *. velocity p
                          }

applySpringDisplacement :: Float -> Particle -> Particle
applySpringDisplacement dt p@(Particle (V2 posX posY) oldPos vel)
  = Particle (V2 newX newY) oldPos vel
  where
    newX = if
      | posX < 0 -> posX - dt * dt * (posX * outsideForce)
      | posX > 1 -> posX + dt * dt * ((1 - posX) * outsideForce)
      | otherwise -> posX
    newY = if
      | posY < 0 -> posY - dt * dt * (posY * outsideForce)
      | otherwise -> posY

clamp (a, b) v = min b (max a v)

nextVelocity :: Float -> Particle -> Particle
nextVelocity dt p = p {velocity = (position p .-. oldPosition p) ./ dt}

infixl 7 *.
f *. (V2 a b) = V2 (f * a) (f * b)

infixl 7 .*
(V2 a b) .* f = V2 (a * f) (b * f)

infixl 7 ./
(V2 a b) ./ f = V2 (a / f) (b / f)

infixl 6 .+.
(V2 a b) .+. (V2 a' b') = V2 (a + a') (b + b')

infixl 6 .-.
(V2 a b) .-. (V2 a' b') = V2 (a - a') (b - b')

type Grid = HashMap.HashMap (Int, Int) [Particle]

relax :: Float -> Particle -> [(Float, V2)] -> Particle
relax dt p qs = p {position = position p .-. deltaPos}
  where
    rho = foldl' (+) 0 (map (\(q, _) -> q * q) qs)
    rhoNear = foldl' (+) 0 (map (\(q, _) -> q * q * q) qs)

    press = k * (rho - rho0)
    pressNear = kNear * rhoNear

    deltaPos = dt * dt *. foldl' (.+.) (V2 0 0) (map (uncurry getDisplacement) qs)

    getDisplacement :: Float -> V2 -> V2
    getDisplacement q rij = (press * q + pressNear * sqr q) *. rij

densityRelaxation :: Float -> Float -> [Particle] -> [Particle]
densityRelaxation dt radius ps = zipWith (relax dt) ps (particulesDensity radius ps)

particulesDensity :: Float -> [Particle] -> [[(Float, V2)]]
particulesDensity radius ps = map (dFriends radius densityGrid) ps
  where
    densityGrid = computeDensityGrid radius ps

dFriends :: Float -> Grid -> Particle -> [(Float, V2)]
dFriends radius grid p = do
          let (x0, y0) = particuleHash radius p
          x <- [x0 - 1..x0 + 1]
          y <- [y0 - 1..y0 + 1]

          part <- fromMaybe [] (HashMap.lookup (x, y) grid)

          let d = distance (position p) (position part)
          guard (d <= radius)
          guard (d /= 0)

          pure (1 - d / radius, (position part .-. position p) ./ d)

distance :: V2 -> V2 -> Float
distance (V2 x y) (V2 x' y') = sqrt (sqr (x - x') + sqr (y - y'))

sqr v = v * v

computeDensityGrid :: Float -> [Particle] -> Grid
computeDensityGrid radius ps = HashMap.fromListWith (\[x] xs -> x:xs) (map (\p -> (particuleHash radius p, [p])) ps)

particuleHash :: Float -> Particle -> (Int, Int)
particuleHash radius p = (truncate (x / radius), truncate (y / radius))
  where
    V2 x y = position p

{-
doubleDensityRelaxation :: Float -> [Particle] -> [Particle]
doubleDensityRelaxation dt particules
-}


main = run
