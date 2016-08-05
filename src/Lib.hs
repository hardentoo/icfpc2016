module Lib
  (
    parseProblem
  , solve
  , unfolds
  )
where

import qualified Paper
import           Problem
import           Solution
import qualified Solver


solve :: Problem -> Maybe Solution
solve = (fmap Paper.toSolution) . Solver.solve . Paper.fromProblem


unfolds :: Problem -> [Problem]
unfolds = fmap Paper.toProblem . Paper.unfolds . Paper.fromProblem
