-- | Problem data types and serialisation
module Problem where

import           Control.Monad      (replicateM)
import           Data.Ratio         (denominator, numerator, (%))
import           GraphTypes
import           Text.Parsec        hiding (State)
import           Text.Parsec.String


data Problem = Problem { probSilhouette :: Silhouette }

data Silhouette = Silhouette { silPoly :: [Polygon]
                             , silSkel :: Skeleton }

data Skeleton = Skeleton { skelEdges :: [Edge] }

data PolygonType = PositivePoly | NegativePoly

data Polygon = Polygon { polygonType     :: PolygonType
                       , polygonVertices :: [Point] }


pointsAreClockwise :: [Point] -> Bool
pointsAreClockwise [] = True
pointsAreClockwise xs@(x:rest) = 0 < sum (uncurry crossProduct <$> zip xs (rest ++ [x]))
  -- shoelace https://en.wikipedia.org/wiki/Shoelace_formula
  where crossProduct (Point (Coord x) (Coord y)) (Point (Coord x') (Coord y')) = (x' - x) * (y + y')


instance Show Problem where
  show (Problem s) = show s

instance Show Silhouette where
  show (Silhouette polys skel) =
    show (length polys) ++ "\n" ++ concatMap show polys ++ show skel

instance Show Polygon where
  show (Polygon polytype verts) =
    show (length sortedVerts) ++ "\n" ++ unlines (show <$> sortedVerts)
    where sortedVerts = case (polytype, pointsAreClockwise verts) of
            (PositivePoly, True) -> reverse verts
            (NegativePoly, False) -> reverse verts
            _ -> verts

instance Show Skeleton where
  show (Skeleton segs) =
    show (length segs) ++ "\n" ++ unlines (show <$> segs)


-- PARSING
parseNat :: Parser Integer
parseNat = read <$> many1 digit <?> "non-negative number"

parseInt :: Parser Integer
parseInt = do
  sign <- try (char '-') <|> pure '+'
  digits <- many1 digit
  (return . (* (if sign == '-' then -1 else 1)) . read $ digits) <?> "number"

parsePolygon :: Parser Polygon
parsePolygon = do
  points <- parseCounted (parsePoint <* newline)
  let polyType = if pointsAreClockwise points then NegativePoly else PositivePoly
  pure $ Polygon polyType points

parseCoord :: Parser Coord
parseCoord = do
  numerator <- parseInt
  denominator <- try (char '/' *> parseNat) <|> pure 1
  pure $ Coord (numerator % denominator)

parsePoint :: Parser Point
parsePoint = Point <$> (parseCoord <* char ',') <*> parseCoord

parseEdge :: Parser Edge
parseEdge = Edge <$> (parsePoint <* space) <*> parsePoint <* newline

parseCounted :: Parser a -> Parser [a]
parseCounted p = do
  count <- parseNat <* newline
  replicateM (fromIntegral count) p

parseSkeleton :: Parser Skeleton
parseSkeleton = Skeleton <$> parseCounted parseEdge

parseSilhouette :: Parser Silhouette
parseSilhouette = Silhouette <$> parseCounted parsePolygon <*> parseSkeleton

parseProblem :: String -> Either ParseError Problem
parseProblem input = Problem <$> runP (parseSilhouette <* eof) () "<string>" input

