module Main where

import           Lib
import           System.Exit (die)

main :: IO ()
main = do
  result <- parseProblem <$> getContents
  case result of
    Right problem -> mapM_ print (unfolds problem)
    Left err -> die (show err)
