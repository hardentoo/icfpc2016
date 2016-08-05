module Lib
where

import           Control.Monad      (replicateM)
import           Data.Ratio         (denominator, numerator, (%))
import           Text.Parsec        hiding (State)
import           Text.Parsec.String


data Problem = Problem Silhouette

data Silhouette = Silhouette [Polygon] Skeleton

data Coord = Coord Rational

data Point = Point { pointX :: Coord
                   , pointY :: Coord }

data Segment = Segment { segmentStart :: Point, segmentEnd :: Point }

data Skeleton = Skeleton [Segment]

data Polygon = Polygon { polygonVertices :: [Point] }

-- data Solution = Solution

-- solve :: Problem -> Solution
-- solve = undefined


instance Show Problem where
  show (Problem s) = show s

instance Show Silhouette where
  show (Silhouette polys skel) =
    show (length polys) ++ "\n" ++ concatMap show polys ++ show skel

instance Show Polygon where
  show (Polygon verts) =
    show (length verts) ++ "\n" ++ unlines (show <$> verts)

instance Show Point where
  show (Point x y) = show x ++ "," ++ show y

instance Show Coord where
  show (Coord amount) = show (numerator amount) ++
    if 1 /= denominator amount then "/" ++ show (denominator amount) else ""

instance Show Skeleton where
  show (Skeleton segs) =
    show (length segs) ++ "\n" ++ unlines (show <$> segs)

instance Show Segment where
  show (Segment start end) = show start ++ " " ++ show end



-- PARSING
parseNat :: Parser Integer
parseNat = read <$> many1 digit <?> "number"

parsePolygon :: Parser Polygon
parsePolygon = Polygon <$> parseCounted (parsePoint <* newline)

parseCoord :: Parser Coord
parseCoord = do
  numerator <- parseNat
  denominator <- try (char '/' *> parseNat) <|> pure 1
  pure $ Coord (numerator % denominator)

parsePoint :: Parser Point
parsePoint = Point <$> (parseCoord <* char ',') <*> parseCoord

parseSegment :: Parser Segment
parseSegment = Segment <$> (parsePoint <* space) <*> parsePoint <* newline

parseCounted :: Parser a -> Parser [a]
parseCounted p = do
  count <- parseNat <* newline
  replicateM (fromIntegral count) p

parseSkeleton :: Parser Skeleton
parseSkeleton = Skeleton <$> parseCounted parseSegment

parseSilhouette :: Parser Silhouette
parseSilhouette = Silhouette <$> parseCounted parsePolygon <*> parseSkeleton

parseProblem :: String -> Either ParseError Problem
parseProblem input = Problem <$> runP (parseSilhouette <* eof) () "<string>" input
