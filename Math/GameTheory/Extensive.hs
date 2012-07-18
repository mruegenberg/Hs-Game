{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Extensive form games
module Math.GameTheory.Extensive (
  module Math.GameTheory.Common
  )
  where

import TypeLevel.NaturalNumber
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Tree as Tree
import Data.Tree(Tree)
import Data.List(intercalate)
import Math.GameTheory.Common
import qualified Math.GameTheory.NormalForm as NF

-- TODO: disambiguate name Outcome in Common and here
-- TODO: maybe somehow enforce that actions fit players, using some kind of dependency?
data (NaturalNumber n) => Game p a n = Outcome (Pos Double n) | Decision p (Map a (Game p a n))
  
(-|>) a b = (a,Outcome b)

infixr 4 -|>

(-|-) a p = \g -> (a,Decision p g)
(|>) a g = a g

infixr 5 -|-
infixr 4 |>

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
           
-- toNormalForm :: Game -> NF.Game

-- Nash Equ
-- Subgame-perfect equs
