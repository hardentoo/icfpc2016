module Manipulation
  (
    mirrorPoints
  )
where

import GraphTypes
import Data.Matrix

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
  normaliser = 1 / ((x' - x)^2 + (y' - y)^2)
  transform = fromLists [ [x^2 - y^2, 2 * x * y], [2 * x * y, y^2 - x^2] ]
