module Paper where

import           Data.List    (nub, union, (\\))
import           Data.Ratio   ((%))
import           GraphTypes
import           Manipulation
import           Problem
import qualified Solution

data Paper = Paper { paperPolygons :: [Polygon] }
  deriving Show

instance Eq Paper where
  p1 == p2 = null $ ldiff `union` rdiff where
    ldiff = (paperPolygons p1) \\ (paperPolygons p2)
    rdiff = (paperPolygons p2) \\ (paperPolygons p1)

polygonsAlongEdge :: Paper -> Edge -> [Polygon]
polygonsAlongEdge (Paper polygons) edge = filter (elem edge . polygonEdges) polygons

unfoldPolygonsAlongEdge :: Paper -> Edge -> [Polygon] -> [Paper]
unfoldPolygonsAlongEdge paper edge polygons = filter isConsistent [moved, retained] where
  moved    = Paper (mirrored ++ (paperPolygons paper \\ polygons))
  retained = Paper (mirrored ++ paperPolygons paper)
  mirrored = map (mirrorPolygon edge) polygons


-- Are we missing some filter here? The restriction is implicitly in "the
-- unfolded result is inconsistent"...
unfoldableEdges :: Paper -> [Edge]
unfoldableEdges = nub . concatMap polygonEdges . paperPolygons

-- This actually isn't all of the possibilities...
unfoldsAlongEdge :: Paper -> Edge -> [Paper]
unfoldsAlongEdge paper edge = concatMap (unfoldPolygonsAlongEdge paper edge) polygonSets
  where
    polygonSets = filter (not . null) . powerset $ (paperPolygons paper)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

unfolds :: Paper -> [Paper]
unfolds paper
  | isFolded paper = nub $ concatMap (unfoldsAlongEdge paper) $ unfoldableEdges paper
  | otherwise = []

unionArea :: Paper -> Rational
unionArea = areaSum . (fmap polygonVertices) . paperPolygons

isFolded :: Paper -> Bool
isFolded = (1 /=) . unionArea

isConsistent :: Paper -> Bool
isConsistent = (1 >=) . unionArea

mirrorPolygon :: Edge -> Polygon -> Polygon
mirrorPolygon edge = Polygon PositivePoly . mirrorPoints edge . polygonVertices

-- The joys of conversion

fromProblem :: Problem -> Paper
fromProblem = Paper . map (Polygon PositivePoly . polygonVertices) . silPoly . probSilhouette

toProblem :: Paper -> Problem
toProblem (Paper polygons) =
  Problem (Silhouette
           ((Polygon PositivePoly) <$> polygonVertices <$> polygons)
           (Skeleton $ concatMap polygonEdges polygons))

toSolution :: Paper -> Solution.Solution
toSolution = undefined


-- some possible example cases
sampleMiniPaper :: Paper
sampleMiniPaper = Paper samplePolygons where
  samplePolygons =
    [
      Polygon
      PositivePoly
        [
          (Point 0 0),
          (Point 0 (Coord (1 % 2))),
          (Point 1 1)
        ]
    ]

-- -- We'd want to exclude holes here, but computational geometry is hard
-- facetiseProblem :: Problem -> [Facet]
-- facetiseProblem (Problem (Silhouette basePolys skeleton)) =
--   _groupIntoFacets (skelEdges skeleton)
