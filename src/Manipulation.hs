module Manipulation
  (
    pointsToMatrix
  )
where

import Problem
import Data.Matrix

pointsToMatrix :: [Point] -> Matrix Rational
pointsToMatrix points = fromLists (map destructure points)

matrixToPoints :: Matrix Rational -> [Point]
matrixToPoints matrix = map restructure (toLists matrix)

destructure :: Point -> [Rational]
destructure (Point (Coord x) (Coord y)) = [x,y]

restructure :: [Rational] -> Point
restructure [x,y] = Point (Coord x) (Coord y)


mirrorPoints :: [Point] -> Segment -> Point
mirrorPoints _ _ = undefined


mirrorMatrix :: Segment -> Matrix Rational
mirrorMatrix (Segment p p') = scaleMatrix normaliser transform where
  Point (Coord x) (Coord y)   = p
  Point (Coord x') (Coord y') = p'
  normaliser = 1 / ((x' - x)^2 + (y' - y)^2)
  transform = fromLists [ [x^2 - y^2, 2 * x * y], [2 * x * y, y^2 - x^2] ]
