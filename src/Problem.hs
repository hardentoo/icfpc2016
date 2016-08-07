-- | Problem data types and serialisation
module Problem
  (
    Problem(..)
  , Silhouette(..)
  , Skeleton(..)
  , showProblem
  , parseProblem
  )
where

import           Control.Monad      (replicateM)
import           Data.Ratio         (denominator, numerator, (%))
import           GraphTypes
import           Text.Parsec        hiding (State)
import           Text.Parsec.String


data Problem = Problem { probSilhouette :: Silhouette }

data Silhouette = Silhouette { silPoly :: [Polygon]
                             , silSkel :: Skeleton }

data Skeleton = Skeleton { skelEdges :: [Edge] }


-- Printing

class ShowProblem a where
  showProblem :: a -> String

instance ShowProblem Problem where
  showProblem (Problem s) = showProblem s

instance ShowProblem Silhouette where
  showProblem (Silhouette polys skel) =
    show (length polys) ++ "\n" ++ concatMap showProblem polys ++ showProblem skel

instance ShowProblem Skeleton where
  showProblem (Skeleton segs) =
    show (length segs) ++ "\n" ++ unlines (showProblem <$> segs)

instance ShowProblem Polygon where
  showProblem (Polygon polytype verts) =
    show (length sortedVerts) ++ "\n" ++ unlines (showProblem <$> sortedVerts)
    where sortedVerts = case (polytype, pointsAreClockwise verts) of
            (PositivePoly, True) -> reverse verts
            (NegativePoly, False) -> reverse verts
            _ -> verts

instance ShowProblem Edge where
  showProblem (Edge start end) = showProblem start ++ " " ++ showProblem end

instance ShowProblem Unit where
  showProblem (Unit amount) = show (numerator amount) ++
    if 1 /= denominator amount then "/" ++ show (denominator amount) else ""

instance ShowProblem Point where
  showProblem (Point x y) = showProblem x ++ "," ++ showProblem y




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

parseUnit :: Parser Unit
parseUnit = do
  numerator <- parseInt
  denominator <- try (char '/' *> parseNat) <|> pure 1
  pure $ Unit (numerator % denominator)

parsePoint :: Parser Point
parsePoint = Point <$> (parseUnit <* char ',') <*> parseUnit

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

