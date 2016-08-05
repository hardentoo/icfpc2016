module Solver
where

import Paper
import Problem
import Solution
import Data.Tree (Tree)
import qualified Data.Tree as T
import Data.Maybe (listToMaybe)


type UnfoldSpace = Tree Paper


exploreUnfolds :: Paper -> Tree Paper
exploreUnfolds paper = T.unfoldTree (\p -> (p, Paper.unfolds p)) paper

solve :: Problem -> Maybe Solution
solve = (fmap toSolution) . listToMaybe . filter (not . isFolded) . T.flatten . exploreUnfolds . fromProblem
