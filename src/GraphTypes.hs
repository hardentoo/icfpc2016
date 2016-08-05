module GraphTypes
where

import           Data.Ratio         (denominator, numerator, (%))


data Edge  = Edge { start :: Point
                  , end :: Point }
  deriving Eq

data Coord = Coord Rational
  deriving Eq

data Point = Point { pointX :: Coord
                   , pointY :: Coord }
  deriving Eq

instance Show Edge where
  show (Edge start end) = show start ++ " " ++ show end

instance Show Coord where
  show (Coord amount) = show (numerator amount) ++
    if 1 /= denominator amount then "/" ++ show (denominator amount) else ""

instance Show Point where
  show (Point x y) = show x ++ "," ++ show y

