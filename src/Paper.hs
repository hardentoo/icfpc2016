module Paper where

import           Data.List    ((\\), nub)
import           GraphTypes
import           Manipulation
import           Problem
import qualified Solution

data Paper = Paper { facets :: [Facet] }

data Facet = Facet { vertices :: [Point] }
  deriving Eq

edgesAboutFacet :: Facet -> [Edge]
edgesAboutFacet (Facet vertices) = map edges pairs where
  pairs = zip vertices (tail $ cycle vertices)
  edges (start, end) = Edge start end

facetsAlongEdge :: Paper -> Edge -> [Facet]
facetsAlongEdge (Paper facets) edge = filter adjacent facets where
  adjacent facet = edge `elem` (edgesAboutFacet facet)

unfoldFacetsAlongEdge :: Paper -> Edge -> [Facet] -> [Paper]
unfoldFacetsAlongEdge (Paper vertices) edge facets = [moved, retained] where
  moved    = Paper (mirrored ++ (vertices \\ facets))
  retained = Paper (mirrored ++ vertices)
  mirrored = map (mirrorFacet edge) facets


-- Are we missing some filter here? The restriction is implicitly in "the
-- unfolded result is inconsistent"...
unfoldableEdges :: Paper -> [Edge]
unfoldableEdges (Paper facets) = nub $ concatMap edgesAboutFacet facets

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

isFolded :: Paper -> Bool
isFolded (Paper facets) = union /= 1 where
  vertices = map (\(Facet vertices) -> vertices) facets
  union = areaSum vertices


mirrorFacet :: Edge -> Facet -> Facet
mirrorFacet edge facet = (Facet mirrored) where
  Facet baseVertices = facet
  mirrored = mirrorPoints edge baseVertices

-- The joys of conversion

fromProblem :: Problem -> Paper
fromProblem = undefined

toProblem :: Paper -> Problem
toProblem = undefined

toSolution :: Paper -> Solution.Solution
toSolution = undefined


-- We'd want to exclude holes here, but computational geometry is hard
facetiseProblem :: Problem -> [Facet]
facetiseProblem (Problem s) = facetiseSilhouette s where
  facetiseSilhouette (Silhouette basePolys skeleton) = concatMap (facetisePolygon skeleton) basePolys

facetisePolygon :: Skeleton -> Polygon -> [Facet]
facetisePolygon (Skeleton edges) (Polygon polyType vertices) = case polyType of
  NegativePoly -> error "Handling empty spaces is probably a horrible rabbithole"
  PositivePoly ->
    error "Still can't do anything really"
