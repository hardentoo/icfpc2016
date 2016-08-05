module Lib
  (
    parseProblem
  , solve
  , unfolds
  )
where

import qualified Paper
import           Problem
import           Solver


unfolds :: Problem -> [Problem]
unfolds = fmap Paper.toProblem . Paper.unfolds . Paper.fromProblem
