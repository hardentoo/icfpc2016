module Paper where

import Problem
import Solution
import Data.List (nub)


data Paper = Paper { facets :: [Facet] }

data Edge  = Edge { start :: Point
                  , end :: Point }
  deriving Eq

data Facet = Facet { vertices :: [Point] }

edgesAboutFacet :: Facet -> [Edge]
edgesAboutFacet (Facet vertices) = map edges pairs where
  pairs = zip vertices (tail $ cycle vertices)
  edges (start, end) = Edge start end

facetsAlongEdge :: Paper -> Edge -> [Facet]
facetsAlongEdge = undefined

unfoldFacetAlongEdge :: Paper -> Edge -> Facet -> [Paper]
unfoldFacetAlongEdge = undefined

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



fromProblem :: Problem -> Paper
fromProblem = undefined

toProblem :: Paper -> Problem
toProblem = undefined

toSolution :: Paper -> Solution
toSolution = undefined
