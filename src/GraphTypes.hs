{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GraphTypes
  (
    Edge(..)
  , squaredEdgeLength
  , Coord(..)
  , Point(..)
  )
where

import           Data.Ratio (denominator, numerator, (%))


data Edge  = Edge { start :: Point
                  , end   :: Point }
  deriving Eq

squaredEdgeLength :: Edge -> Coord
squaredEdgeLength e = squaredDistanceBetween (start e) (end e)

newtype Coord = Coord { coordVal :: Rational }
  deriving (Eq, Num, Ord)

data Point = Point { pointX :: Coord
                   , pointY :: Coord }
  deriving (Eq, Ord)

squaredDistanceBetween :: Point -> Point -> Coord
squaredDistanceBetween p1 p2 = sq (pointX p2 - pointX p1) + sq (pointY p2 - pointY p1)

sq :: Num a => a -> a
sq a = a * a

instance Show Edge where
  show (Edge start end) = show start ++ " " ++ show end

instance Show Coord where
  show (Coord amount) = show (numerator amount) ++
    if 1 /= denominator amount then "/" ++ show (denominator amount) else ""

instance Show Point where
  show (Point x y) = show x ++ "," ++ show y

