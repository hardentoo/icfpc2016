module Solution where

import           Data.Function (on)
import           Data.List     (intercalate)
import           Data.Maybe    (fromJust)
import           GraphTypes

data Vertex = Vertex { vertInitialPosition :: Point
                     , vertFinalPosition   :: Point }

data Facet = Facet { facetVertices :: [Vertex] }

data Solution = Solution { solFacets :: [Facet] }

instance Eq Vertex where
  (==) = (==) `on` vertInitialPosition

instance Ord Vertex where
  compare = compare `on` vertInitialPosition

instance Show Solution where
  show (Solution facets) =
    show (length allVertices) ++ "\n" ++
    unlines (show . vertInitialPosition <$> allVertices) ++
    show (length facets) ++ "\n" ++
    unlines (showFacet <$> facets) ++
    unlines (show . vertFinalPosition <$> allVertices)
    where
      allVertices = concatMap facetVertices facets
      vertexIndexMap = zip allVertices [0..]
      showFacet (Facet verts) =
        intercalate " " ( show (length verts):
                          (show . fromJust . (flip lookup vertexIndexMap) <$> verts) )


exampleSolution = Solution
  [ Facet
    [ Vertex (Point (Coord 0) (Coord 1)) (Point (Coord 1) (Coord 1))
    , Vertex (Point (Coord 1) (Coord 1)) (Point (Coord 1) (Coord 0))
    , Vertex (Point (Coord 1) (Coord 0)) (Point (Coord 0) (Coord 0))
    , Vertex (Point (Coord 0) (Coord 0)) (Point (Coord 0) (Coord 1))
    ]
  ]
