module Manipulation
  (
    mirrorPoints
  , areaSum
  )
where

import           Data.Matrix
import           Data.Ratio  ((%))
import           GraphTypes

pointsToMatrix :: [Point] -> Matrix Rational
pointsToMatrix points = fromLists (map destructure points)

matrixToPoints :: Matrix Rational -> [Point]
matrixToPoints matrix = map restructure (toLists matrix)

destructure :: Point -> [Rational]
destructure (Point (Unit x) (Unit y)) = [x,y]

restructure :: [Rational] -> Point
restructure [x,y] = Point (Unit x) (Unit y)

mirrorPoints :: Edge -> [Point] -> [Point]
mirrorPoints edge points = matrixToPoints mirroredMatrix where
  mirroredMatrix = (pointsToMatrix points) * (mirrorMatrix edge)

mirrorMatrix :: Edge -> Matrix Rational
mirrorMatrix (Edge (Point (Unit x) (Unit y)) (Point (Unit x') (Unit y'))) =
  scaleMatrix normaliser transform
  where
    dx = x - x'
    dy = y - y'
    normaliser = 1 / (dx^2 + dy^2)
    transform = fromLists [ [dx^2 - dy^2, 2 * dx * dy], [2 * dx * dy, dy^2 - dx^2] ]

-- This could overcount, but hey
areaSum :: [[Point]] -> Rational
areaSum polygons = sum . (map polygonSize) $ polygons

polygonSize :: [Point] -> Rational
polygonSize points = (sum . map unsignedSize) matrices where
  matrices = map pointsToMatrix pairs
  unsignedSize matrix = abs $ 1 % 2 * (detLU matrix)
  pairs    = [[x,y] | (x,y) <- zip points (tail $ cycle points)]
