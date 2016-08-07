module Manipulation
  (
    mirrorPolygon
  , traceMirroring
  , areaSum
  )
where

import           Data.Matrix
import           Data.Ratio  ((%))
import           GraphTypes
import           Data.List (sortBy, minimumBy)
import           Data.Function (on)
import           Data.Ord (comparing)


pointsToMatrix :: [Point] -> Matrix Rational
pointsToMatrix points = fromLists (map destructure points)

matrixToPoints :: Matrix Rational -> [Point]
matrixToPoints matrix = map restructure (toLists matrix)

destructure :: Point -> [Rational]
destructure (Point (Unit x) (Unit y)) = [x,y]

restructure :: [Rational] -> Point
restructure [x,y] = Point (Unit x) (Unit y)

mirrorPolygon :: Edge -> Polygon -> Polygon
mirrorPolygon edge = Polygon PositivePoly . mirrorPoints edge . polygonVertices

traceMirroring :: Polygon -> Polygon -> [(Point, Point)]
traceMirroring from to = zip (polygonVertices from) (polygonVertices to)

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

{- Fetch the (approximate) convex hull, using that to find the corner points
Having constructed those, evaluate the matrix that transforms the resulting
square down to the origin space...
  ## origin * alignmentMatrix = hull
  => alignmentMatrix = inv(origin) * hull
The space is dramatically overdetermined – select perpendicular vectors (well,
if we're lucky – more work here ideally)
-}
alignmentMatrix :: Polygon -> Matrix Rational
alignmentMatrix origin = originBase * (hullMat origin) where
  originBase = fromLists [[0,1], [1,0]]
  hullMat    = pointsToMatrix . transformPoints . polygonVertices

originMatrix :: Matrix Rational
originMatrix = fromLists [[0,1], [1,1], [1,0], [0,0]]

transformPoints :: [Point] -> [Point]
transformPoints = (\(a:_:b:_) -> [a,b]) . clampedConvexHull

clampedConvexHull :: [Point] -> [Point]
clampedConvexHull points = [ closestIn hullPoint points | hullPoint <- ahull ] where
  ahull = (approxConvexHull points) :: [Point]

closestIn :: Point -> [Point] -> Point
closestIn target original = minimumBy (comparator target) original where
  comparator target = comparing (distanceSq target)
  distanceSq p1 p2 = dx^2 + dy^2 where
    [x , y ] = destructure p1 :: [Rational]
    [x', y'] = destructure p2 :: [Rational]
    dx = x - x' :: Rational
    dy = y - y' :: Rational


-------------------

approxConvexHull :: [Point] -> [Point]
approxConvexHull points
    | length points >= 3 = scan [pt0] rests
    | otherwise       = points
    where
        -- Find the most bottom-left point pt0
        pt0 = foldr bottomLeft (Point (Unit (2^128)) (Unit (2^128))) points where
            bottomLeft pa pb = case ord of
                               LT -> pa
                               GT -> pb
                               EQ -> pa
                       where ord = (compare `on` (\ (Point x y) -> (y, x))) pa pb

        -- Sort other points based on angle
        rests = tail (sortBy (compare `on` compkey pt0) points) where
            compkey (Point (Unit x0) (Unit y0)) (Point (Unit x) (Unit y)) = (atan2 (fromRational (y - y0)) (fromRational (x - x0)),
                                       abs (x - x0))

        -- Scan the points to find out convex
        -- -- handle the case that all points are collinear
        scan [p0] (p1:ps)
            | isTurned pz p0 p1 == STRAIGHT = [pz, p0]
            where pz = last ps

        scan (x:xs) (y:z:rsts) = case isTurned x y z of
            RIGHT    -> scan xs (x:z:rsts)
            STRAIGHT -> scan (x:xs) (z:rsts) -- skip collinear points
            LEFT     -> scan (y:x:xs) (z:rsts)

        scan xs [z] = z : xs

data Direction = LEFT | RIGHT | STRAIGHT
               deriving (Show, Eq)

isTurned :: Point -> Point -> Point -> Direction
isTurned (Point ax ay) (Point bx by) (Point cx cy) = case sign of
    EQ -> STRAIGHT
    LT -> RIGHT
    GT -> LEFT
    where sign = compare ((bx - ax) * (cy - ay)) ((cx - ax) * (by - ay))
