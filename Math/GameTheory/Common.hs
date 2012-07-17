{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

-- | Common definitions
module Math.GameTheory.Common(
  (<\>)
  )
  where

import TypeLevel.NaturalNumber
import Math.GameTheory.Internal.Common
  
-- | Constructing outcomes in a game. 
class (NaturalNumber n) => GamePos a n | n -> a where
  (<\>) :: Double -> a -> (Pos Double n)
  
infixr 5 <\>

instance GamePos Double N2 where
  (<\>) a b = Pos [a,b] n2

instance (NaturalNumber n) => GamePos (Pos Double (SuccessorTo (SuccessorTo n))) (SuccessorTo (SuccessorTo (SuccessorTo n))) where
  (<\>) d (Pos ds n) = Pos (d : ds) (successorTo n)