{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
module GraphTypes
  (
    Edge(..)
  , edgesMeet
  , squaredEdgeLength
  , Unit(..)
  , Point(..)
  , pointsAreClockwise
  , PolygonType(..)
  , Polygon(..)
  , polygonEdges
  , edgesToPolygons
  )
where

import           Control.Applicative (empty)
import           Data.Function       (on)
import           Data.List           (delete, intersect, nub, sort, union)
import           Data.Maybe          (catMaybes, fromJust)
import           Data.Ratio          (denominator, numerator, (%))
import           Data.Tree           (Tree)
import qualified Data.Tree           as T

data Edge  = Edge { start :: Point
                  , end   :: Point }
           deriving Show

instance Eq Edge where
  Edge a b == Edge a' b' = (a, b) == (a', b') || (a, b) == (b', a')

instance Ord Edge where
  compare (Edge a b) (Edge a' b') = (compare `on` sort) [a, b] [a', b']

edgesMeet :: Edge -> Edge -> Bool
edgesMeet (Edge a b) (Edge a' b') = not (null (intersect [a, b] [a', b']))

squaredEdgeLength :: Num a => Edge -> Rational
squaredEdgeLength e = squaredDistanceBetween (start e) (end e)

newtype Unit = Unit { unitVal :: Rational }
  deriving (Eq, Num, Ord, Show)

data Point = Point { pointX :: Unit
                   , pointY :: Unit }
  deriving (Eq, Ord, Show)

squaredDistanceBetween :: Point -> Point -> Rational
squaredDistanceBetween p1 p2 = unitVal $ sq (pointX p2 - pointX p1) + sq (pointY p2 - pointY p1)

sq :: Num a => a -> a
sq a = a * a

data PolygonType = PositivePoly | NegativePoly
  deriving (Eq, Show)

data Polygon = Polygon { polygonType     :: PolygonType
                       , polygonVertices :: [Point] }
             deriving Show

polygonEdges :: Polygon -> [Edge]
polygonEdges (Polygon _ vertices) = (uncurry Edge) <$> zip vertices (tail $ cycle vertices)

instance Eq Polygon where
  p1 == p2 = (polygonType p1 == polygonType p2) && (e1 == nub (e1 ++ e2))
    where
      e1 = polygonEdges p1
      e2 = polygonEdges p2

edgesToPolygons :: PolygonType -> [Edge] -> [Polygon]
edgesToPolygons ptype edges = nub $ (Polygon ptype . init . edgesToPoints) <$> edgeLoops edges

edgesToPoints :: [Edge] -> [Point]
edgesToPoints [] = []
edgesToPoints [(Edge a b)] = [a, b]
edgesToPoints ((Edge a b):e2:es) =
  case joinEdge b e2 of
    Just p -> [a, b, p] ++ joins p es
    Nothing -> case joinEdge a e2 of
                 Just p -> [b, a, p] ++ joins p es
  where
    joins :: Point -> [Edge] -> [Point]
    joins _ [] = []
    joins p (e:es) = nxt : joins nxt es
      where nxt = fromJust (joinEdge p e)

joinEdge :: Point -> Edge -> Maybe Point
joinEdge p (Edge a b) | p == a = Just b
joinEdge p (Edge a b) | p == b = Just a
joinEdge _ _ = Nothing

edgeLoops :: [Edge] -> [[Edge]]
edgeLoops edges = catMaybes $ concatMap T.flatten $ T.unfoldForest edgeGroupings [([e], delete e edges) | e <- edges ]

edgesFormLoop :: [Edge] -> Bool
edgesFormLoop (e:_:es@(_:_)) = edgesMeet e (last es)
edgesFormLoop _ = False

edgeGroupings :: ([Edge], [Edge]) -> (Maybe [Edge], [([Edge], [Edge])])
edgeGroupings (sofar, nexts) =
  if edgesFormLoop sofar then (Just sofar, [])
  else (Nothing,) $ do
    next <- nexts
    let remaining = delete next nexts
    if next `edgesMeet` (head sofar)
    then return (next:sofar, remaining)
    else if next `edgesMeet` (last sofar)
    then return (sofar ++ [next], remaining)
    else empty


pointsAreClockwise :: [Point] -> Bool
pointsAreClockwise [] = True
pointsAreClockwise xs@(x:rest) = 0 < sum (uncurry crossProduct <$> zip xs (rest ++ [x]))
  -- shoelace https://en.wikipedia.org/wiki/Shoelace_formula
  where crossProduct (Point (Unit x) (Unit y)) (Point (Unit x') (Unit y')) = (x' - x) * (y + y')


