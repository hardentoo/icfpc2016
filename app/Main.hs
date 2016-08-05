module Main where

import System.IO (hPrint, stderr)
import           Lib
import           System.Exit (die)

main :: IO ()
main = do
  result <- parseProblem <$> getContents
  case result of
    Right problem -> case solve problem of
                       Just solution -> print solution
                       Nothing -> error "no solution found"
    Left err -> die (show err)
