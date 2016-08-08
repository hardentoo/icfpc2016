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
  , splitAtIntersections
  )
where

import           Control.Applicative (empty)
import           Data.Function       (on)
import           Data.List           (delete, intersect, nub, sort, tails,
                                      union, (\\))
import           Data.Maybe          (catMaybes, fromJust, fromMaybe,
                                      listToMaybe)
import           Data.Ratio          (denominator, numerator, (%))
import           Data.Tree           (Tree)
import qualified Data.Tree           as T

data Edge  = Edge { start :: Point
                  , end   :: Point }

instance Eq Edge where
  Edge a b == Edge a' b' = (a, b) == (a', b') || (a, b) == (b', a')

instance Ord Edge where
  compare (Edge a b) (Edge a' b') = (compare `on` sort) [a, b] [a', b']

instance Show Edge where
  show (Edge x y) = "Edge (" ++ show x ++ " => " ++ show y ++ ")"

edgesMeet :: Edge -> Edge -> Bool
edgesMeet (Edge a b) (Edge a' b') = not (null (intersect [a, b] [a', b']))

squaredEdgeLength :: Num a => Edge -> Rational
squaredEdgeLength e = squaredDistanceBetween (start e) (end e)

newtype Unit = Unit { unitVal :: Rational }
  deriving (Eq, Num, Fractional, Ord)

instance Show Unit where
  show (Unit v) = show (numerator v) ++
    (if 1 == (denominator v) then "" else "/" ++ show (denominator v))

data Point = Point { pointX :: Unit
                   , pointY :: Unit }
  deriving (Eq, Ord)

instance Show Point where
  show (Point x y) = "Point (" ++ show x ++ "," ++ show y ++ ")"

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


splitAtIntersections :: [Edge] -> [Edge]
splitAtIntersections edges =
  let pairs = [(e1, e2) | e1 <- edges, e2 <- (delete e1 edges)]
      allSplits = uncurry edgesSplitAtIntersections <$> pairs
      pairsWithSplits = filter (not . null . snd) $ zip pairs allSplits
  in
    case listToMaybe pairsWithSplits of
      Just ((e1, e2), splits) -> splitAtIntersections ((edges \\ [e1, e2]) ++ splits)
      Nothing -> edges

edgesSplitAtIntersections :: Edge -> Edge -> [Edge]
edgesSplitAtIntersections e1@(Edge a b) e2@(Edge a' b') =
  if edgesMeet e1 e2 then []
  else case edgeIntersection e1 e2 of
         Just point -> Edge point <$> filter (point /=) [a, b, a', b']
         _          -> []

-- http://stackoverflow.com/a/1968345
edgeIntersection :: Edge -> Edge -> Maybe Point
edgeIntersection (Edge (Point p0_x p0_y) (Point p1_x p1_y)) (Edge (Point p2_x p2_y) (Point p3_x p3_y)) =
    let
      s1_x = p1_x - p0_x
      s1_y = p1_y - p0_y
      s2_x = p3_x - p2_x
      s2_y = p3_y - p2_y
      det = (-s2_x * s1_y + s1_x * s2_y)
      s = (-s1_y * (p0_x - p2_x) + s1_x * (p0_y - p2_y)) / det
      t = ( s2_x * (p0_y - p2_y) - s2_y * (p0_x - p2_x)) / det
    in
    if (det /= 0 && s >= 0 && s <= 1 && t >= 0 && t <= 1)
    then Just $ Point (p0_x + (t * s1_x)) (p0_y + (t * s1_y))
    else Nothing


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
                 _ -> error "unconnected edge"
  where
    joins :: Point -> [Edge] -> [Point]
    joins _ [] = []
    joins p (e:es) = nxt : joins nxt es
      where nxt = fromJust (joinEdge p e)


sampleEdges = [ Edge (Point 0 0)     (Point (1/2) 0)
              , Edge (Point 0 0)     (Point 0     (2/3))
              , Edge (Point (1/2) 0) (Point (1/2) (2/3))
              , Edge (Point 0 (1/3)) (Point (1/2) (1/3))
              , Edge (Point 0 (2/3)) (Point (1/2) (2/3))]


joinEdge :: Point -> Edge -> Maybe Point
joinEdge p (Edge a b) | p == a = Just b
joinEdge p (Edge a b) | p == b = Just a
joinEdge _ _ = Nothing

edgeLoops :: [Edge] -> [[Edge]]
edgeLoops edges = catMaybes $ concatMap T.flatten $ T.unfoldForest edgeGroupings [([e], delete e edges) | e <- edges ]

edgesFormCorrectLoop :: [Edge] -> Bool
edgesFormCorrectLoop (e:_:es@(_:_)) = edgesMeet e (last es)
edgesFormCorrectLoop _ = False

edgesFormBadLoop :: [Edge] -> Bool
edgesFormBadLoop edges = any edgesFormCorrectLoop (tails (tail edges))

edgeGroupings :: ([Edge], [Edge]) -> (Maybe [Edge], [([Edge], [Edge])])
edgeGroupings (sofar, nexts) =
  if edgesFormCorrectLoop sofar then (Just sofar, [])
  else if edgesFormBadLoop sofar then (Nothing, [])
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


