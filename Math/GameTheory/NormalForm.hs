-- | Representation and algorithms for normal form games

-- TODO: don't export the constructor for games! force people to use the <\> construct

module Math.GameTheory.NormalForm (
  module Math.GameTheory.Common,
  Game,Player,Action,Strategy,
  dims, -- FIXME: maybe don't export?
  utility,playerUtil,
  expUtility
  )
  where

import Math.GameTheory.Internal.NormalForm
import Math.GameTheory.Common
import TypeLevel.NaturalNumber
import Data.Array

-- | A player. Should not be 0, and not exceed the number of players in a game where it is used.
type Player = Int

-- | A mixed strategy. Should sum up to 1, and have length equal to the number of actions for the corresponding player
type Strategy = [Double]

-- | A specific action for some player
type Action = Int

dims :: (NaturalNumber n, Ord n) => Game n -> Pos Int n
dims (Game arr) = snd $ bounds arr

-- | The utility for a specific position in the game
utility :: (NaturalNumber n, Ord n) => Game n -> Pos Action n -> Pos Double n
utility (Game arr) pos = arr ! pos

-- | Extract the utility for a specific player from the utilities for all player at a position
playerUtil :: (NaturalNumber n) => Pos Double n -> Player -> Double
playerUtil (Pos l n) p = l !! (p - 1)

-- a default position. for use with replaceAt
defaultPos :: (NaturalNumber n, Ord n) => Game n -> (Pos Int n)
defaultPos game = dims game

replacePos ::  (NaturalNumber n) => (Pos Int n) -> Int -> Int -> (Pos Int n)
replacePos (Pos l n) i v = Pos (replaceAt v l i) n

replaceAt :: a -> [a] -> Int -> [a]
replaceAt _ [] _ = []
replaceAt y (x:xs) 0 = y:xs
replaceAt y (x:xs) i = x : (replaceAt y xs (i - 1))

-- | Expected utility of some player for a strategy profile of all players
expUtility :: (NaturalNumber n, Ord n) => Game n -> Player -> Pos Strategy n -> Double
expUtility g@(Game arr) player (Pos strategies n) = go 0 (defaultPos g) strategies
  where -- go :: (NaturalNumber n, Ord n) => Pos Int n -> [t] -> Double
        go j pos [] = playerUtil (utility g pos) player
        go j pos (x:xs) = sum $ map (\(i,y) -> y * (go (j + 1) (replacePos pos j i) xs)) (zip [1..] x)