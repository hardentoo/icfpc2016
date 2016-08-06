module Lib
  (
    parseProblem
  , solve
  , unfolds
  , unfoldsToLevel
  )
where

import qualified Data.Tree as T
import qualified Paper
import           Problem
import           Solution
import qualified Solver


solve :: Problem -> Maybe Solution
solve = (fmap Paper.toSolution) . Solver.solve . Paper.fromProblem


unfolds :: Problem -> [Problem]
unfolds = fmap Paper.toProblem . Paper.unfolds . Paper.fromProblem

unfoldsToLevel :: Int -> Problem -> [Problem]
unfoldsToLevel level = fmap Paper.toProblem . concat . take (level + 1) . T.levels. Solver.exploreUnfolds . Paper.fromProblem
