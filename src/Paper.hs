module Paper where

import           Data.List    (nub, (\\))
import           Data.Ratio   ((%))
import           GraphTypes
import           Manipulation
import           Problem
import qualified Solution

data Paper = Paper { paperFacets :: [Facet] }
  deriving Show

data Facet = Facet { facetVertices :: [Point] }
  deriving (Eq, Show)

edgesAboutFacet :: Facet -> [Edge]
edgesAboutFacet (Facet vertices) = map (uncurry Edge) pairs
  where
    pairs = zip vertices (tail $ cycle vertices)

facetsAlongEdge :: Paper -> Edge -> [Facet]
facetsAlongEdge (Paper facets) edge = filter adjacent facets where
  adjacent facet = edge `elem` (edgesAboutFacet facet)

unfoldFacetsAlongEdge :: Paper -> Edge -> [Facet] -> [Paper]
unfoldFacetsAlongEdge paper edge facets = filter isConsistent [moved, retained] where
  moved    = Paper (mirrored ++ (paperFacets paper \\ facets))
  retained = Paper (mirrored ++ paperFacets paper)
  mirrored = map (mirrorFacet edge) facets


-- Are we missing some filter here? The restriction is implicitly in "the
-- unfolded result is inconsistent"...
unfoldableEdges :: Paper -> [Edge]
unfoldableEdges = nub . concatMap edgesAboutFacet . paperFacets

-- This actually isn't all of the possibilities...
unfoldsAlongEdge :: Paper -> Edge -> [Paper]
unfoldsAlongEdge paper edge = concatMap (unfoldFacetsAlongEdge paper edge) facetSets where
  facets    = facetsAlongEdge paper edge
  facetSets = powerset facets

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

unfolds :: Paper -> [Paper]
unfolds paper = concatMap (unfoldsAlongEdge paper) $ unfoldableEdges paper

unionArea :: Paper -> Rational
unionArea = areaSum . (fmap facetVertices) . paperFacets

isFolded :: Paper -> Bool
isFolded = (1 /=) . unionArea

isConsistent  :: Paper -> Bool
isConsistent = (1 >=) . unionArea

mirrorFacet :: Edge -> Facet -> Facet
mirrorFacet edge = Facet . mirrorPoints edge . facetVertices

-- The joys of conversion

fromProblem :: Problem -> Paper
fromProblem = Paper . map (Facet . polygonVertices) . silPoly . probSilhouette

toProblem :: Paper -> Problem
toProblem (Paper facets) =
  Problem (Silhouette
           ((Polygon PositivePoly) <$> facetVertices <$> facets)
           (Skeleton $ concatMap edgesAboutFacet facets))

toSolution :: Paper -> Solution.Solution
toSolution = undefined


-- some possible example cases
sampleMiniPaper :: Paper
sampleMiniPaper = Paper sampleFacets where
  sampleFacets =
    [
      Facet
        [
          (Point 0 0),
          (Point 0 (Coord (1 % 2))),
          (Point 1 1)
        ]
    ]

-- We'd want to exclude holes here, but computational geometry is hard
facetiseProblem :: Problem -> [Facet]
facetiseProblem (Problem s) = facetiseSilhouette s where
  facetiseSilhouette (Silhouette basePolys skeleton) = concatMap (facetisePolygon skeleton) basePolys

facetisePolygon :: Skeleton -> Polygon -> [Facet]
facetisePolygon (Skeleton edges) (Polygon polyType vertices) = case polyType of
  NegativePoly -> error "Handling empty spaces is probably a horrible rabbithole"
  PositivePoly ->
    error "Still can't do anything really"
