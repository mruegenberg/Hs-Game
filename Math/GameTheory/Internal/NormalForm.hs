{-# LANGUAGE GADTs #-}

module Math.GameTheory.Internal.NormalForm (
  module Math.GameTheory.Common,
  module Math.GameTheory.Internal.Common,
  Game(..),
  insertPos
  )
  where

import Math.GameTheory.Common
import Math.GameTheory.Internal.Common
import Data.Array
import TypeLevel.NaturalNumber


-- | The main game type. Usually, use the `mkGame` functions to construct a game.
data Game n where 
  Game :: (NaturalNumber n) => (Array (Pos Int n) (Pos Double n)) -> Game n

-- FIXME: this is not yet right for games with more than 2 dimensions
instance (NaturalNumber n, Ord n) => Show (Game n) where
  show (Game arr) = dropWhile (== '\n') (concatMap mapPos (assocs arr))
    where mapPos ((Pos idx _),vals) = (if null sp then sp else sp ++ "|") ++ " " ++ (show vals) ++ " |"
            where sp = spaces (tail idx)
          spaces = foldr (\i l -> if i == 1 then '\n' : l else l) []


insertPos :: (NaturalNumber n) => (Pos a n) -> Int -> a -> Pos a (SuccessorTo n)
insertPos (Pos l n) i v = Pos (insertAt v l (i-1)) (successorTo n)
  where insertAt x (l':lst) n' | n' > 0 = l' : (insertAt x lst (n'-1))
        insertAt x lst _ = x : lst