-- | Public interface for the library

module Lib
  (
    parseProblem
  , solve
  , unfolds
  , unfoldsToLevel
  , historyOfUnfolding
  )
where

import qualified Data.Tree as T
import qualified Paper
import           Problem
import           Solution
import qualified Solver


solve :: Problem -> Maybe Solution
solve = (fmap (Paper.toSolution . head)) . Solver.solve . Paper.fromProblem


unfolds :: Problem -> [Problem]
unfolds = fmap Paper.toProblem . Paper.unfolds . Paper.fromProblem


unfoldsToLevel :: Int -> Problem -> [Problem]
unfoldsToLevel level = fmap (Paper.toProblem . head) . concat . take (level + 1) . T.levels. Solver.exploreUnfolds . Paper.fromProblem


historyOfUnfolding :: Int -> Problem -> [Problem]
historyOfUnfolding idx = fmap (Paper.toProblem) . reverse . (!! idx) . concat . T.levels. Solver.exploreUnfolds . Paper.fromProblem
