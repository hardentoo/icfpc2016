module Solver
where

import Problem
import Data.Graph.AStar
import Data.Set (Set)

unfurl :: Silhouette -> Set Silhouette
unfurl _ = undefined
{-
  for each polygon in the silhouette:
    for each edge in the polygon:
      each of:
        | replicate polygon, reflected across the edge
        +--| leave skeleton intact
        |  | trim skeleton
        | *move* polygon, reflected across the edge
        +--| leave skeleton intact
        |  | trim skeleton
        = filter:
          in some sense coherent (e.g. total area, adjacency...)
-}
