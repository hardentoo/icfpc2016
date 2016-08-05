module Paper where

import Problem
import Solution
import GraphTypes
import Data.List (nub, delete)

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

unfoldFacetAlongEdge :: Paper -> Edge -> Facet -> [Paper]
unfoldFacetAlongEdge (Paper vertices) edge facet = [moved, retained] where
  moved    = Paper (mirrored:(delete facet vertices))
  retained = Paper (mirrored:vertices)
  mirrored = mirrorFacet edge facet


-- Are we missing some filter here? The restriction is implicitly in "the
-- unfolded result is inconsistent"...
unfoldableEdges :: Paper -> [Edge]
unfoldableEdges (Paper facets) = nub $ concatMap edgesAboutFacet facets

unfoldsAlongEdge :: Paper -> Edge -> [Paper]
unfoldsAlongEdge paper edge = concatMap (unfoldFacetAlongEdge paper edge) facets
  where facets = facetsAlongEdge paper edge

unfolds :: Paper -> [Paper]
unfolds paper = concatMap (unfoldsAlongEdge paper) $ unfoldableEdges paper

isFolded :: Paper -> Bool
isFolded = undefined


mirrorFacet :: Edge -> Facet -> Facet
mirrorFacet = undefined


fromProblem :: Problem -> Paper
fromProblem = undefined

toProblem :: Paper -> Problem
toProblem = undefined

toSolution :: Paper -> Solution
toSolution = undefined
