{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GraphTypes
  (
    Edge(..)
  , squaredEdgeLength
  , Unit(..)
  , Point(..)
  , pointsAreClockwise
  , PolygonType(..)
  , Polygon(..)
  , polygonEdges
  )
where

import           Data.Function (on)
import           Data.List     (intersect, nub, union, sort)
import           Data.Ratio    (denominator, numerator, (%))


data Edge  = Edge { start :: Point
                  , end   :: Point }

instance Eq Edge where
  Edge a b == Edge a' b' = (a, b) == (a', b') || (a, b) == (b', a')

instance Ord Edge where
  compare (Edge a b) (Edge a' b') = (compare `on` sort) [a, b] [a', b']

edgesMeet :: Edge -> Edge -> Bool
edgesMeet (Edge a b) (Edge a' b') = not (null (intersect [a, b] [a', b']))

squaredEdgeLength :: Num a => Edge -> Rational
squaredEdgeLength e = squaredDistanceBetween (start e) (end e)

newtype Unit = Unit { unitVal :: Rational }
  deriving (Eq, Num, Ord)

data Point = Point { pointX :: Unit
                   , pointY :: Unit }
  deriving (Eq, Ord)

squaredDistanceBetween :: Point -> Point -> Rational
squaredDistanceBetween p1 p2 = unitVal $ sq (pointX p2 - pointX p1) + sq (pointY p2 - pointY p1)

sq :: Num a => a -> a
sq a = a * a

data PolygonType = PositivePoly | NegativePoly
  deriving Eq

data Polygon = Polygon { polygonType     :: PolygonType
                       , polygonVertices :: [Point] }

polygonEdges :: Polygon -> [Edge]
polygonEdges (Polygon _ vertices) = (uncurry Edge) <$> zip vertices (tail $ cycle vertices)

instance Eq Polygon where
  p1 == p2 = (polygonType p1 == polygonType p2) && (e1 == nub (e1 ++ e2))
    where
      e1 = polygonEdges p1
      e2 = polygonEdges p2

pointsAreClockwise :: [Point] -> Bool
pointsAreClockwise [] = True
pointsAreClockwise xs@(x:rest) = 0 < sum (uncurry crossProduct <$> zip xs (rest ++ [x]))
  -- shoelace https://en.wikipedia.org/wiki/Shoelace_formula
  where crossProduct (Point (Unit x) (Unit y)) (Point (Unit x') (Unit y')) = (x' - x) * (y + y')


instance Show Polygon where
  show (Polygon polytype verts) =
    show (length sortedVerts) ++ "\n" ++ unlines (show <$> sortedVerts)
    where sortedVerts = case (polytype, pointsAreClockwise verts) of
            (PositivePoly, True) -> reverse verts
            (NegativePoly, False) -> reverse verts
            _ -> verts

instance Show Edge where
  show (Edge start end) = show start ++ " " ++ show end

instance Show Unit where
  show (Unit amount) = show (numerator amount) ++
    if 1 /= denominator amount then "/" ++ show (denominator amount) else ""

instance Show Point where
  show (Point x y) = show x ++ "," ++ show y


