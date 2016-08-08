module Paper where

import           Data.List    (nub, union, (\\))
import           Data.Ratio   ((%))
import           GraphTypes
import           Manipulation
import           Problem
import qualified Solution

data Paper = Paper
  {
    paperPolygons  :: [Polygon]
  , paperMovements :: [(Point, Point)]
  }
  deriving Show

instance Eq Paper where
  p1 == p2 = null $ ldiff `union` rdiff where
    ldiff = paperPolygons p1 \\ paperPolygons p2
    rdiff = paperPolygons p2 \\ paperPolygons p1

polygonsAlongEdge :: Paper -> Edge -> [Polygon]
polygonsAlongEdge paper edge = filter (elem edge . polygonEdges) (paperPolygons paper)

unfoldPolygonsAlongEdge :: Paper -> Edge -> [Polygon] -> [Paper]
unfoldPolygonsAlongEdge paper edge polygons = filter isConsistent [retained] where
  --moved    = Paper (mirrored ++ (paperPolygons paper \\ polygons))
  retained = Paper (mirrored ++ paperPolygons paper) (changes ++ paperMovements paper)
  changes  = concat $ zipWith traceMirroring polygons mirrored :: [(Point, Point)]
  mirrored = map (mirrorPolygon edge) polygons

-- Are we missing some filter here? The restriction is implicitly in "the
-- unfolded result is inconsistent"...
unfoldableEdges :: Paper -> [Edge]
unfoldableEdges = nub . concatMap polygonEdges . paperPolygons

-- This actually isn't all of the possibilities...
unfoldsAlongEdge :: Paper -> Edge -> [Paper]
unfoldsAlongEdge paper edge = concatMap (unfoldPolygonsAlongEdge paper edge) polygonSets
  where
    polygonSets = filter (not . null) . powerset $ paperPolygons paper

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

unfolds :: Paper -> [Paper]
unfolds paper
  | isFolded paper = nub $ concatMap (unfoldsAlongEdge paper) $ unfoldableEdges paper
  | otherwise = []

unionArea :: Paper -> Rational
unionArea = areaSum . fmap polygonVertices . paperPolygons

isFolded :: Paper -> Bool
isFolded = (1 /=) . unionArea

isOversize :: Paper -> Bool
isOversize = (1 <) . unionArea

fitsInBounds :: Paper -> Bool
fitsInBounds paper = (maxX - minX) <= 1 && (maxY - minY) <= 1 where
  vertices = concatMap polygonVertices . paperPolygons $ paper
  xPoints  = map pointX vertices
  yPoints  = map pointY vertices
  (minX, maxX) = (minimum xPoints, maximum xPoints)
  (minY, maxY) = (minimum yPoints, maximum yPoints)

isConsistent :: Paper -> Bool
isConsistent paper = and [ predicate paper | predicate <- predicates ] where
  predicates = [isOversize, fitsInBounds]

-- The joys of conversion

fromProblem :: Problem -> Paper
fromProblem = (flip Paper []) . edgesToPolygons PositivePoly . skelEdges . silSkel . probSilhouette

toProblem :: Paper -> Problem
toProblem paper =
  Problem (Silhouette
           ((Polygon PositivePoly) <$> polygonVertices <$> polygons)
           (Skeleton $ concatMap polygonEdges polygons)) where
    polygons = paperPolygons paper

toSolution :: Paper -> Solution.Solution
toSolution = undefined

-- some possible example cases
sampleMiniPaper :: Paper
sampleMiniPaper = Paper samplePolygons [] where
  samplePolygons =
    [
      Polygon
      PositivePoly
        [
          Point 0 0,
          Point 0 (Unit (1 % 2)),
          Point 1 1
        ]
    ]
