module Main where

import           Data.Maybe         (fromMaybe, listToMaybe)
import           Lib
import           System.Environment (getArgs)
import           System.Exit        (die)

main :: IO ()
main = do
  depth <- read . fromMaybe "1" . listToMaybe <$> getArgs
  result <- parseProblem <$> getContents
  case result of
    Right problem -> mapM_ print $ unfoldsToLevel depth problem
    Left err -> die (show err)
