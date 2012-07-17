-- | Definitions for internal use.

module Math.GameTheory.Internal.Common (
  Pos(..)
  ) where

import Data.Ix
import Data.List(elemIndex)
import TypeLevel.NaturalNumber

data (NaturalNumber n) => Pos a n = Pos [a] n deriving (Ord, Eq, Show)

instance (Ix a, NaturalNumber n, Ord n) => Ix (Pos a n) where
  range (Pos g1 n, Pos g2 _) = go n (zipWith (curry range) g1 g2)
    where go :: (NaturalNumber n) => n -> [[a]] -> [Pos a n]
          go n [] = [Pos [] n]
          go n (x : xs) = concatMap (\i -> map (\(Pos g n) -> Pos (i : g) n) (go n xs)) x
  index (g1, g2) g3 = case elemIndex g3 (range (g1, g2)) of -- FIXME: inefficient
    (Just i) -> i
    Nothing -> 0
  inRange (Pos g1 n, Pos g2 _) (Pos g3 _) = (and (zipWith (<=) g1 g3)) && (and (zipWith (<=) g3 g2))