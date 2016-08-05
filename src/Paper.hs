module Paper where

import Problem
import Solution


data Paper

data Edge

data Facet

facetsAlongEdge :: Paper -> Edge -> [Facet]
facetsAlongEdge = undefined

unfoldFacetAlongEdge :: Paper -> Edge -> Facet -> [Paper]
unfoldFacetAlongEdge = undefined

unfoldableEdges :: Paper -> [Edge]
unfoldableEdges = undefined

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
