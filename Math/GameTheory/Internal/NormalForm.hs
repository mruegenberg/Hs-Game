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
data (NaturalNumber n) => Game n = Game (Array (Pos Int n) (Pos Double n))
          deriving Show
-- TODO: Proper (custom) "Show" Instance?
-- TODO: Eq instance? (Needed?)


insertPos :: (NaturalNumber n) => (Pos a n) -> Int -> a -> Pos a (SuccessorTo n)
insertPos (Pos l n) i v = Pos (insertAt v l (i-1)) (successorTo n)
  where insertAt x (l':lst) n' | n' > 0 = l' : (insertAt x lst (n'-1))
        insertAt x lst _ = x : lst