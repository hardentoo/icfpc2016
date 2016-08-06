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


facetEdges :: Facet -> [Edge]
facetEdges (Facet vertices) = (uncurry Edge) <$> zip vertices (tail $ cycle vertices)

facetsAlongEdge :: Paper -> Edge -> [Facet]
facetsAlongEdge (Paper facets) edge = filter (elem edge . facetEdges) facets

unfoldFacetsAlongEdge :: Paper -> Edge -> [Facet] -> [Paper]
unfoldFacetsAlongEdge paper edge facets = filter isConsistent [moved, retained] where
  moved    = Paper (mirrored ++ (paperFacets paper \\ facets))
  retained = Paper (mirrored ++ paperFacets paper)
  mirrored = map (mirrorFacet edge) facets


-- Are we missing some filter here? The restriction is implicitly in "the
-- unfolded result is inconsistent"...
unfoldableEdges :: Paper -> [Edge]
unfoldableEdges = nub . concatMap facetEdges . paperFacets

-- This actually isn't all of the possibilities...
unfoldsAlongEdge :: Paper -> Edge -> [Paper]
unfoldsAlongEdge paper edge = concatMap (unfoldFacetsAlongEdge paper edge) facetSets
  where
    facetSets = powerset $ facetsAlongEdge paper edge

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

unfolds :: Paper -> [Paper]
unfolds paper
  | isFolded paper = concatMap (unfoldsAlongEdge paper) $ unfoldableEdges paper
  | otherwise = []

unionArea :: Paper -> Rational
unionArea = areaSum . (fmap facetVertices) . paperFacets

isFolded :: Paper -> Bool
isFolded = (1 /=) . unionArea

isConsistent :: Paper -> Bool
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
           (Skeleton $ concatMap facetEdges facets))

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

-- -- We'd want to exclude holes here, but computational geometry is hard
-- facetiseProblem :: Problem -> [Facet]
-- facetiseProblem (Problem (Silhouette basePolys skeleton)) =
--   _groupIntoFacets (skelEdges skeleton)
