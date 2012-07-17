module Math.GameTheory.Internal.NormalForm (
  module Math.GameTheory.Common,
  module Math.GameTheory.Internal.Common,
  Game(..)
  )
  where

import Math.GameTheory.Common
import Math.GameTheory.Internal.Common
import Data.Array
import TypeLevel.NaturalNumber


-- | Usually, construct a game using the `mkGame` functions.
data (NaturalNumber n) => Game n = Game (Array (Pos Int n) (Pos Double n))
          deriving Show
-- TODO: Proper (custom) "Show" Instance?
-- TODO: Eq instance? (Needed?)