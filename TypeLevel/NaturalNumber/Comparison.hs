{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

{- Partly based on http://www.haskell.org/haskellwiki/Type_arithmetic -}

{-| 
  Type-level comparison for type level natural numbers. 
  Probably requires the FlexibleContexts extension.

  Usage: `foo :: (NaturalNumber n, NaturalNumber m, Cmp n m Equal) => TagList n Int -> TagList m Int -> Bool`.
  This will enforce that both TagLists are of equal length.
  
  You can use Equal, Less and Greater.

  A future update might add or replace these with Equal, LessEq and GreaterEq. 
  (Both approaches have advantages and disadavantages.)

-}



module TypeLevel.NaturalNumber.Comparison where

import TypeLevel.NaturalNumber

data Less
data Equal
data Greater

-- maybe add functional dependencies to help the typechecker? might not be necessary though.
class (NaturalNumber n, NaturalNumber m) => Cmp n m a

instance Cmp Zero Zero Equal
instance (NaturalNumber n) => Cmp Zero (SuccessorTo n) Less
instance (NaturalNumber n) => Cmp (SuccessorTo n) Zero Greater
instance (NaturalNumber n, NaturalNumber m, Cmp n m c) => Cmp (SuccessorTo n) (SuccessorTo m) c
