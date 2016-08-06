module Main where

import           Data.Maybe         (fromMaybe, listToMaybe)
import           Lib
import           System.Environment (getArgs)
import           System.Exit        (die)

main :: IO ()
main = do
  depthArg <- fromMaybe "1" . listToMaybe <$> getArgs
  result <- parseProblem <$> getContents
  case result of
    Right problem ->
      mapM_ print $ case depthArg of
                      ('@':rest) -> historyOfUnfolding (read rest) problem
                      _          -> unfoldsToLevel (read depthArg) problem
    Left err -> die (show err)
