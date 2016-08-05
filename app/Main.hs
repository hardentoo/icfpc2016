module Main where

import           Lib
import           System.Exit (die)

main :: IO ()
main = do
  result <- parseProblem <$> getContents
  case result of
    Right problem -> print problem -- print (solve problem)
    Left err -> die (show err)
