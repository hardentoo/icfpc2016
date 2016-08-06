module Manipulation
  (
    mirrorPoints
  , areaSum
  )
where

import GraphTypes
import Data.Matrix
import Data.Ratio   ((%))

pointsToMatrix :: [Point] -> Matrix Rational
pointsToMatrix points = fromLists (map destructure points)

matrixToPoints :: Matrix Rational -> [Point]
matrixToPoints matrix = map restructure (toLists matrix)

destructure :: Point -> [Rational]
destructure (Point (Coord x) (Coord y)) = [x,y]

restructure :: [Rational] -> Point
restructure [x,y] = Point (Coord x) (Coord y)

mirrorPoints :: Edge -> [Point] -> [Point]
mirrorPoints edge points = matrixToPoints mirroredMatrix where
  mirroredMatrix = (pointsToMatrix points) * (mirrorMatrix edge)

mirrorMatrix :: Edge -> Matrix Rational
mirrorMatrix (Edge p p') = scaleMatrix normaliser transform where
  Point (Coord x) (Coord y)   = p
  Point (Coord x') (Coord y') = p'
  dx = x - x'
  dy = y - y'
  normaliser = 1 / ((x' - x)^2 + (y' - y)^2)
  transform = fromLists [ [dx^2 - dy^2, 2 * dx * dy], [2 * dx * dy, dy^2 - dx^2] ]

-- This could overcount, but hey
areaSum :: [[Point]] -> Rational
areaSum polygons = sum . (map polygonSize) $ polygons

polygonSize :: [Point] -> Rational
polygonSize points = (sum . map unsignedSize) matrices where
  matrices = map pointsToMatrix pairs
  unsignedSize matrix = abs $ 1 % 2 * (detLU matrix)
  pairs    = [[x,y] | (x,y) <- zip points (tail $ cycle points)]
