module Solver
where

import           Data.Maybe (listToMaybe)
import           Data.Tree  (Tree)
import qualified Data.Tree  as T
import           Paper


-- Each node in the resulting tree carries a current paper and its
-- previous folded states
exploreUnfolds :: Paper -> Tree [Paper]
exploreUnfolds paper = T.unfoldTree (\ps@(p:_) -> (ps, (:ps) <$> Paper.unfolds p)) [paper]

-- Find a solution together with its previous folded states
solve :: Paper -> Maybe [Paper]
solve = listToMaybe . filter (not . isFolded . head) . T.flatten . exploreUnfolds
