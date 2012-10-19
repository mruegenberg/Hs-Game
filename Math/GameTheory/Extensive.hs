{-# LANGUAGE NoMonomorphismRestriction #-}

{-| Perfect-information extensive form games

Constructing a game:

>  testGame = Decision "P1" $ Map.fromList $ [
>    ("a1" -|-"P2"|> Map.fromList [
>       ("b1" -|-"P1"|> Map.fromList ["d1" -|> -1 <\> -1, "d2" -|> 0 <\> 1]),
>       ("b2" -|-"P1"|> Map.fromList ["e1" -|> 0 <\> 0,"e2" -|> 2 <\> 0])
>     ]),
>     ("a2" -|-"P2"|> Map.fromList [
>       ("c1" -|-"P1"|> Map.fromList ["f1" -|> 1 <\> 0,"f2" -|> 0 <\> 0]),
>       ("c2" -|-"P1"|> Map.fromList ["g1" -|> 1 <\> 1,"g2" -|> 0 <\> 2])
>     ])
>   ]

In this game, player P1 first decides between a1 and a2. 

If a1 was chosen, P2 decides between b1 and b2 and afterwards, P1 chooses between d1 and d2 (if b1 was chosen) or e1 and e2 (if b2 was chosen).

Otherwise, if a2 was chosen, P2 decides between c1 and c2, then, depending on what P2 chose, P1 decides between f1 and f2 or g1 and g2.

At this point, there is no further branching, and we get to the payoffs/outcomes.
-}

module Math.GameTheory.Extensive (
  module Math.GameTheory.Common,
  (-|>), (-|-), (|>),
  Strategy,
  toNormalForm,
  subgamePerfect, pureNash
  )
  where

import TypeLevel.NaturalNumber
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Tree as Tree
import Data.Tree(Tree)
import Math.GameTheory.Common
import Math.GameTheory.Internal.Common
import qualified Math.GameTheory.NormalForm as NF

-- TODO: disambiguate name Outcome in Common and here
-- TODO: maybe somehow enforce that actions fit players, using some kind of dependency?
data (NaturalNumber n) => Game p a n = Outcome (Pos Double n) | Decision p (Map a (Game p a n))

-- | Map a choice to an outcome. Returns the choice and the resulting game.
-- (An outcome is simply a utility value for each player.)
(-|>) :: NaturalNumber n => t -> Pos Double n -> (t, Game p a n)
(-|>) a b = (a,Outcome b)

infixr 4 -|>

{-| Use in conjunction with '|>' to form a complete arrow. 

@\"a\"-|-\"P1\"|>mp@ means that if action a is chosen, the next subgame is a choice for P1 between the alternatives in the map mp.
-}
(-|-) :: NaturalNumber n => t -> p -> Map a (Game p a n) -> (t, Game p a n)
(-|-) a p = \g -> (a,Decision p g)

-- | Use in conjunction with '-|-'.
(|>) :: (a -> b) -> a -> b
(|>) a g = a g

infixr 5 -|-
infixr 4 |>

-- | A pure strategy. Maps players to lists of actions. The actions should exactly correspond to the different possibilities in the game.
type Strategy p a = Map p [a]

{-
toTree :: (NaturalNumber n, Show p) => Game p a n -> Tree String
toTree (Outcome utilities) = Tree.Node ("[" ++ (intercalate "," (map show utilities)) ++ "]") []
toTree (Decision p children) = Tree.Node (show p) $ map (\(a,g) -> toTree g) (Map.assocs children)
-}
-- TODO: define -|> in a way that allows to build a Decision node as well
testGame :: Game String String N2
testGame = Decision "P1" $ Map.fromList $ [
  ("a1" -|-"P2"|> Map.fromList [
      ("b1" -|-"P1"|> Map.fromList ["d1" -|> -1 <\> -1, "d2" -|> 0 <\> 1]),
      ("b2" -|-"P1"|> Map.fromList ["e1" -|> 0 <\> 0,"e2" -|> 2 <\> 0])
      ]),
  ("a2" -|-"P2"|> Map.fromList [
      ("c1" -|-"P1"|> Map.fromList ["f1" -|> 1 <\> 0,"f2" -|> 0 <\> 0]),
      ("c2" -|-"P1"|> Map.fromList ["g1" -|> 1 <\> 1,"g2" -|> 0 <\> 2])
      ])
  ]
           
-- | Convert to a normal-form game
-- | Note that the resulting normal-form game may be quite large
toNormalForm :: (NaturalNumber n) => Game p a n -> NF.Game n
toNormalForm _ = undefined

-- | Find the (pure) subgame-perfect equilibrium in the game.
subgamePerfect :: (NaturalNumber n) => Game p a n -> Strategy p a
subgamePerfect _ = undefined

-- | Nash Equilibria
pureNash :: (NaturalNumber n) => Game p a n -> [Strategy p a]
pureNash = undefined
