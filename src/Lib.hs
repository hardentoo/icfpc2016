module Lib
where

import           Control.Monad      (replicateM)
import           Data.Ratio         ((%))
import           Text.Parsec        hiding (State)
import           Text.Parsec.String


data Problem = Problem Silhouette
  deriving Show

data Silhouette = Silhouette [Polygon] Skeleton
  deriving Show

type Coord = Rational

data Point = Point { pointX :: Coord
                   , pointY :: Coord }
  deriving Show

data Segment = Segment { segmentStart :: Point, segmentEnd :: Point }
  deriving Show

data Skeleton = Skeleton [Segment]
  deriving Show

data Polygon = Polygon { polygonVertices :: [Point] }
  deriving Show

data Solution = Solution
  deriving Show

solve :: Problem -> Solution
solve = undefined



-- PARSING
parseNat :: Parser Int
parseNat = read <$> many1 digit <?> "number"

parsePolygon :: Parser Polygon
parsePolygon = do
  vertexCount <- parseNat <* newline
  Polygon <$> replicateM vertexCount (parsePoint <* newline)

parseCoord :: Parser Coord
parseCoord = do
  numerator <- parseNat
  denominator <- try (char '/' *> parseNat) <|> pure 1
  pure (fromIntegral numerator % fromIntegral denominator)

parsePoint :: Parser Point
parsePoint = Point <$> (parseCoord <* char ',') <*> parseCoord

parseSegment :: Parser Segment
parseSegment = Segment <$> (parsePoint <* space) <*> parsePoint <* newline

parseSkeleton :: Parser Skeleton
parseSkeleton = do
  segmentCount <- parseNat <* newline
  Skeleton <$> replicateM segmentCount parseSegment

parseSilhouette :: Parser Silhouette
parseSilhouette = do
  polyCount <- parseNat <* newline
  Silhouette <$> replicateM polyCount parsePolygon
             <*> parseSkeleton

parseProblem :: String -> Either ParseError Problem
parseProblem input = Problem <$> runP (parseSilhouette <* eof) () "<string>" input
