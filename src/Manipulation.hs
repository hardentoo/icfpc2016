module Manipulation
  (
    pointsToMatrix
  )
where

import Problem
import Data.Matrix

pointsToMatrix :: [Point] -> Matrix Rational
pointsToMatrix points = fromLists lists where
  destructure (Point (Coord x) (Coord y)) = [x,y]
  lists = map destructure points

matrixToPoints :: Matrix Rational -> [Point]
matrixToPoints matrix = map restructure lists where
  restructure [x,y] = Point (Coord x) (Coord y)
  lists = toLists matrix
