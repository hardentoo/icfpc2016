module Solver
where

import           Data.Maybe (listToMaybe)
import           Data.Tree  (Tree)
import qualified Data.Tree  as T
import           Paper


type UnfoldSpace = Tree Paper


exploreUnfolds :: Paper -> Tree Paper
exploreUnfolds paper = T.unfoldTree (\p -> (p, Paper.unfolds p)) paper

solve :: Paper -> Maybe Paper
solve = listToMaybe . filter (not . isFolded) . T.flatten . exploreUnfolds
