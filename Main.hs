{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FunctionalDependencies #-}

-- | Solution concepts for two-player games
module Main where

import Numeric.LinearProgramming
import Data.Array
import Data.List(nub, elemIndex)
import TypeLevel.NaturalNumber
import Math.GameTheory.NormalForm

import Math.GameTheory.Common
import Math.GameTheory.Internal.NormalForm

main = undefined

-- | Shortcuts for 2 and 3 player games
rowPlayer = 1
colPlayer = 2
matPlayer = 3

-- | Easily build a two-player game by giving a two-dimensional list
mkGame2 :: [[Pos Double N2]] -> Game N2
mkGame2 payoffs =
  Game (listArray (Pos [1,1] n2,Pos [dimRow, dimCol] n2) (concat payoffs))
    where dimRow = (length payoffs)
          dimCol = length $ head payoffs

-- | Easily build a three-player game with a three-dimensional list
mkGame3 :: [[[Pos Double N3]]] -> Game N3
mkGame3 payoffs =
  Game $ listArray (Pos [1,1,1] n3, Pos [dimRow, dimCol, dimMat] n3) (concatMap concat payoffs)
    where dimRow = length $ head payoffs
          dimCol = length $ head $ head payoffs
          dimMat = length $ payoffs

yankPos :: (NaturalNumber n) => (Pos Int (SuccessorTo n)) -> Int -> Pos Int n
yankPos (Pos l n) i = Pos (snd (yank (i - 1) l)) (predecessorOf n)

emptyPos :: Pos a N0
emptyPos = Pos [] n0

insertPos :: (NaturalNumber n) => (Pos a n) -> Int -> a -> Pos a (SuccessorTo n)
insertPos (Pos l n) i v = Pos (insertAt v l (i-1)) (successorTo n)
  where insertAt v l 0 = v : l
        insertAt v (l:ls) n = l : (insertAt v ls (n-1))
        
appendPos :: (NaturalNumber n) => (Pos a n) -> a -> Pos a (SuccessorTo n)
appendPos g@(Pos l n) v = insertPos g (length l) v
        
yank :: Int -> [a] -> (a, [a])
yank 0 (x:xs) = (x, xs)
yank n l =
    let (a, (b:c)) = splitAt n l
    in  (b, (a ++ c))

-- | Takes a game and a player, returns the security level as well as the maximin mixed strategy for the player
maxiMin :: (NaturalNumber n, Ord n) => Game (SuccessorTo n) -> Player -> (Double, Strategy)
maxiMin game player = (secLevel, probabilities)
  where (secLevel, probs') = case (simplex problem constraints []) of
          Optimal r -> r
          _ -> undefined
        probabilities = tail probs'
        problem     = Maximize (1 : (take ownD zeros))
        constraints = Dense $
                      ((0 : (take ownD (repeat 1))) :==: 1) : -- all probabilities add up to 1
                      (map (nonzero ownD) [1..ownD]) ++  -- all probabilities are greater than 0
                      (map constraint otherDs) -- [-1, ...] :=>: 0 for each row/column
        nonzero d i = ((take i zeros) ++ (1 : (take (d - i) zeros))) :=>: 0
        (ownD,otherDs',n) = case dims game of
          (Pos ds n) -> case yank (player - 1) ds of (a,b) -> (a,b,n)
          
        constraint pos' = ((-1) : map (\ownIdx -> playerUtil 
                                                 (utility game (insertPos pos' player ownIdx)) player)
                           [1..ownD])
                          :=>: 0
        otherDs = map (\d -> Pos d (predecessorOf n)) $ go $ map (\i -> [1..i]) otherDs'
        go [] = [[]]
        go (x:xs) = concatMap (\y -> map (\z -> z : y) x) (go xs)                                                 
        zeros     = repeat 0
        
-- | Checks if an action of a player is dominated in the game
dominated :: (NaturalNumber n, Ord n) => Game (SuccessorTo n) -> Player -> Action -> Bool
dominated game@(Game arr) player action = val < 1
  where (val, probs') = case simplex problem constraints [] of
          Optimal r -> r
          _ -> undefined
          
        -- variables in the LP are s_i(b_i), and their sum should be minimized
        problem = Minimize (take ownD $ repeat 1)
        
        constraints = Dense $
                      (map (nonzero ownD) [1..ownD]) ++ -- s_i(b_i) >= 0 forall b_i
                      (map constraint otherDs)
                      
        nonzero d i = ((take i zeros) ++ (1 : (take (d - i) zeros))) :=>: 0
        zeros       = repeat 0
                      
        constraint pos' = (map (\ownIdx -> playerUtil (utility game pos) player) [1..ownD])
                          :=>: (playerUtil (utility game pos) player)
            where 
              pos = (insertPos pos' player action)
                      
        go [] = [[]]
        go (x:xs) = concatMap (\y -> map (\z -> z : y) x) (go xs)  
        otherDs           = map (\d -> Pos d (predecessorOf n)) $ go $ map (\i -> [1..i]) otherDs'
        (ownD,otherDs',n) = case dims game of
          (Pos ds n') -> case yank (player - 1) ds of (a,b) -> (a,b,n')
                                                        
-- | Eliminate a given action for a given player for the game
-- action should be a possible action, ideally statically.
eliminate :: (NaturalNumber n, Ord n) => Game n -> (Player, Action) -> Game n
eliminate (Game arr) (player, action) = 
  Game (listArray (lowerBound,upperBound') (map (arr !) filteredIndices))
  where
    filteredIndices = filter (\(Pos i _) -> (i !! (player - 1)) /= action) (indices arr)
    (lowerBound,Pos upperBound n) = bounds arr
    (restBounds',playerBound:restBounds'') = splitAt (player - 1) upperBound
    upperBound' = Pos (restBounds' ++ ((playerBound - 1) : restBounds'')) n

-- | Iterated strict dominance. Returns for each player the list of dominated actions
iteratedDominance :: (NaturalNumber n, Ord n) => Game (SuccessorTo n) -> Pos [Action] (SuccessorTo n)
iteratedDominance origGame = iteratedDominance' origGame
  where
    iteratedDominance' game = Pos dominatedActions'' n
      where
        (dimensions,n) = case dims game of (Pos ds n') -> (ds,n')
        actions = map (\d -> [1..d]) dimensions
        dominatedActions = map (\(i,actions') -> (i,filter (dominated game i) actions')) $ 
                           zip [1..] actions 
        gameEliminatedActions = foldl 
                                (\g (i,actions') -> elimActions g i actions') 
                                game dominatedActions
        elimActions g i actions' = foldl (\g' action -> eliminate g' (i, action)) g actions'
        (Pos dominatedActions' _) = iteratedDominance' gameEliminatedActions
        dominatedActions'' = zipWith 
                             (\dAs dAs' -> dAs ++ (shift dAs dAs')) 
                             (map snd dominatedActions) dominatedActions'
        shift dAs dAs' = map (\i -> i + (length (filter (<= i) dAs))) dAs'

-- | Compute the optimal strategy and expected utility for the leader in a Stackelberg Game
stackelbergMixedCommitment :: Game N2 -> Player -> (Double, Strategy)
stackelbergMixedCommitment game leader = 
  foldr 
  (\a_2 (u,s) -> case optim a_2 of
      Just (u',s') | u' > u -> (u',s')
      _ -> (u,s))
  (- (1/0.0),take otherD (repeat 1)) 
  otherActions
  where 
    (ownActions,otherActions) = ([1..ownD],[1..otherD])
    (ownD,otherD,follower) = case (leader,dims game) of
      (1, Pos [d1,d2] _) -> (d1,d2, 2)
      (2, Pos [d1,d2] _) -> (d2,d1, 1)
      _ -> error "Invalid player in two-person game."
    
    optim a_2 = case simplex (problem a_2) (constraints a_2) varBounds of
      Optimal r -> Just r
      _         -> Nothing
      
    varBounds = map (\i -> i :=>: 0) [1..ownD]
    problem a_2 = Maximize $ map (\a_1 -> leaderUtil $ utility game (mkPos a_1 a_2)) ownActions
    mkPos a_1 a_2 = case leader of 
      1 -> Pos [a_1,a_2] n2
      2 -> Pos [a_2,a_2] n2
      _ -> error "Invalid player in two-person game."
    constraints a_2 = Dense $
                      ((map (\_ -> 1) ownActions) :==: 1) : 
                      (map (constraint a_2) otherActions)
                      
    constraint a_2 a_2' = (map (\a_1 -> (followerUtil $ utility game (mkPos a_1 a_2)) -  
                                       (followerUtil $ utility game (mkPos a_1 a_2')))
                           ownActions)
                          :=>: 0
    
    leaderUtil utils = playerUtil utils leader
    followerUtil utils = playerUtil utils follower
    
    
--------------------
    
stackelberg1 :: Game N2
stackelberg1 = mkGame2 [[1 <\> 3, 3 <\> 2],
                        [0 <\> 0, 2 <\> 1]]
               
stackelberg2 :: Game N2
stackelberg2 = mkGame2 [[0 <\> 3, 3 <\> 2],
                        [1 <\> 0, 2 <\> 1]]
                                                        
game22a :: Game N3
game22a = mkGame3 [ [ [1 <\> 1 <\> 0, 0 <\> 1 <\> 1],
                      [0 <\> 1 <\> 1, 1 <\> 0 <\> 1] ],
                    [ [0 <\> 1 <\> 1, 1 <\> 0 <\> 1],
                      [1 <\> 0 <\> 1, 1 <\> 1 <\> 0] ]
                  ]
g2Game :: Game N2
g2Game = mkGame2 [ [1 <\> 1, 0 <\> 2, 2 <\> 0, 0 <\> 2],
                   [2 <\> 0, 1 <\> 1, 0 <\> 2, 2 <\> 0],
                   [0 <\> 2, 2 <\> 0, 1 <\> 1, 0 <\> 2], 
                   [2 <\> 0, 0 <\> 2, 2 <\> 0, 1 <\> 1]
                 ]

{-
A = {2,3,4,5,6}
	2	3	4	5	6
2	0,0	-1,1	2,-2	-1,1	2,-2
3	1,-1	0,0	-1,1	-1,1	2,-2
4	-2,2	1,-1	0,0	-1,1	-1,1
5	1,-1	1,-1	1,-1	0,0	-1,1
6	-2,2	-2,2	1,-1	1,-1	0,0
-}
g25Game :: Game N2
g25Game = mkGame2 [[   0 <\>   0 , (-1) <\>   1 ,   2  <\> (-2), (-1) <\>   1 ,   2  <\> (-2)],
                   [   1 <\> (-1),   0  <\>   0 , (-1) <\>   1 , (-1) <\>   1 ,   2  <\> (-2)],
                   [(-2) <\>   2 ,   1  <\> (-1),   0  <\>   0 , (-1) <\>   1 , (-1) <\>   1 ],
                   [   1 <\> (-1),   1  <\> (-1),   1  <\> (-1),   0  <\>   0 , (-1) <\>   1 ],
                   [(-2) <\>   2 , (-2) <\>   2 ,   1  <\> (-1),   1  <\> (-1),   0  <\>   0 ]
                  ]


         
g27TestGame :: Game N2
g27TestGame = mkGame2 [[0 <\> 0, (-1) <\> 1, 1 <\> (-1), (-1) <\> 1],
                       [1 <\> (-1), 0 <\> 0, (-1) <\> 1, 1 <\> (-1)],
                       [(-1) <\> 1, 1 <\> (-1), 0 <\> 0, (-1) <\> 1],
                       [1 <\> (-1), (-1) <\> 1, 1 <\> (-1), 0 <\> 0]]

g30Game :: Game N2
g30Game = mkGame2 [[4 <\> 2, 5 <\> 1],
                   [3 <\> 3, 1 <\> 2],
                   [5 <\> 1, 4 <\> 3]]

g31Game :: Game N3
g31Game = mkGame3 [ [ [1 <\> 1 <\> 0, 1 <\> -2 <\> -1],
                      [1 <\> 1 <\> 0, 1 <\> -2 <\> -1]],
                    [ [0 <\> -1 <\> -1, 0 <\> 0 <\> 1],
                      [0 <\> -1 <\> -1, 0 <\> 0 <\> 1]]]
              
{-
  
-- | Iterated strict dominance
-- | Returns the list of dominated actions for the row and column players
iteratedDominance :: Game -> ([Action],[Action])
iteratedDominance origGame = iteratedDominance' origGame
  where
    iteratedDominance' game = if null dominatedRows && null dominatedCols then (dominatedRows, dominatedCols) else
                                (dominatedRows ++ dominatedRows'', dominatedCols ++ dominatedCols'')
      where
        (dC,dR) = dims game
        dominatedRows = filter (dominated game RowPlayer) [1..dR]
        dominatedCols = filter (dominated game ColPlayer) [1..dC]
        elimRowsGame = foldl (\g action -> eliminate g (RowPlayer, action)) game dominatedRows
        elimColsGame = foldl (\g action -> eliminate g (ColPlayer, action)) elimRowsGame dominatedCols
        (dominatedRows',dominatedCols') = iteratedDominance' elimColsGame
        dominatedRows'' = map (\i -> i + (length (filter (<= i) dominatedRows))) dominatedRows'
        dominatedCols'' = map (\i -> i + (length (filter (<= i) dominatedCols))) dominatedCols'
        
        
{- 
in each step, find all dominated actions in rows and columns and eliminate them;
call iterated dominance on the reduced game, obtaining the dominated actions;
[transform the dominated actions from the reduced game so that they correspond to the correct numbers in the full game]
-}

-- | Find pure strategy nash equilibria. The result is a list of 2-tuples of actions, each corresponding to an equilibrium.
pureNash :: Game -> [(Action,Action)]
pureNash (Game arr) = undefined

{-
-- | Find (some?) mixed strategy nash equilibria
-- By using the "others should be indifferent" play.
-- Further equilibria by allowing players to mix only over some nonempty subset of their actions.
-- That by the way should also yield the pure equilibria since the only way to mix over a single action is to play it.
-- see http://oyc.yale.edu/sites/default/files/mixed_strategies_handout_0.pdf
-- Note that there are all kinds of fancy algorithms for this.
mixedNash :: Game -> [(Strategy, Strategy)]
mixedNash (Game arr) = undefined

-- | Pure and mixed strategy nash equilibria
-- based on support enumeration. 
-- parallel algorithm for n player game: http://www.cs.wayne.edu/~dgrosu/pub/cse09.pdf
-- also need to handle degenerate games?
-- check and handle special cases for zero sum games (NEs are computable faster)
-- 
-- note: for code blocks in documentation, do 
{- |
@foo
bar@
-}
-- also works @inline@
nash :: Game -> [(Strategy, Strategy)]
nash game = nub $ (map (\(a1,a2) -> (rowActionToStrategy a1, 
                                    colActionToStrategy a2)) 
                   (pureNash game)) ++ 
            (mixedNash game)
  where rowActionToStrategy a = undefined
        colActionToStrategy a = undefined
-}

                      

        
tstGame = mkGame [ [3 <\> 1, 0 <\> 0],
                   [0 <\> 3, 3 <\> 2],
                   [1 <\> 1, 1 <\> 2] ]
          
tstGame' = mkGame [ [3 <\> 1, 0 <\> 0],
                    [0 <\> 3, 3 <\> 2]]
           
tstGame'' = mkGame [ [3 <\> 1],
                     [0 <\> 3] ]

tstGame3 = mkGame [ [3 <\> 1, 0 <\> 0, 0 <\> 0],
                    [1 <\> 1, 1 <\> 2, 5 <\> 0],
                    [0 <\> 1, 4 <\> 0, 0 <\> 0] ]
           
iterDomTest = tstGame3
           
tstGame3' = mkGame [ [3 <\> 1, 0 <\> 0],
                     [1 <\> 1, 1 <\> 2],
                     [0 <\> 1, 4 <\> 0] ]
            
tstGame3'' = mkGame [ [3 <\> 1, 0 <\> 0],
                      [0 <\> 1, 4 <\> 0] ]

tstGame3''' = mkGame [ [3 <\> 1],
                       [0 <\> 1] ]
          
iterWeakDomTest = mkGame [ [2 <\> 1, 1 <\> 1, 0 <\> 0],
                           [1 <\> 1, 1 <\> 2, 0 <\> 1],
                           [0 <\> 0, 1 <\> 0, 1 <\> 1] ]
                  
game18a = mkGame [ [2 <\> 2, 0 <\> 1],
                   [1 <\> 1, 3 <\> 3] ]
          
game18b a = mkGame [ [1 <\> 1, a <\> 0],
                     [0 <\> 0, 2 <\> 1] ]
              
main = print $ dominated tstGame RowPlayer 1
        
{-
RowPlayer: fst,
each column leads to an inequality,

ColPlayer: snd,
each row leads to an inequality
-}                      
          

gGame = mkGame [ [(0,0),(3,1)]
               , [(1,3),(0,0)]
               ]
         
-- main = print $ maxiMin gGame RowPlayer



g1Game = mkGame [ [1 <\> 1, 0 <\> 2, 2 <\> 0],
                  [2 <\> 0, 1 <\> 1, 0 <\> 2],
                  [0 <\> 2, 2 <\> 0, 1 <\> 1] ]
         
g2Game = mkGame [ [1 <\> 1, 0 <\> 2, 2 <\> 0, 0 <\> 2],
                  [2 <\> 0, 1 <\> 1, 0 <\> 2, 2 <\> 0],
                  [0 <\> 2, 2 <\> 0, 1 <\> 1, 0 <\> 2], 
                  [2 <\> 0, 0 <\> 2, 2 <\> 0, 1 <\> 1]
                ]
         
g3Game = mkGame [ [1 <\> 1, 0 <\> 2, 2 <\> 0, 0 <\> 2],
                  [2 <\> 0, 1 <\> 1, 0 <\> 2, 0 <\> 2],
                  [0 <\> 2, 2 <\> 0, 1 <\> 1, 0 <\> 2], 
                  [2 <\> 0, 2 <\> 0, 2 <\> 0, 1 <\> 1]
                ]
         
g4Game = mkGame [ [1 <\> 1, 2 <\> 0, 2 <\> 0, 0 <\> 2],
                  [0 <\> 2, 1 <\> 1, 0 <\> 2, 0 <\> 2],
                  [0 <\> 2, 2 <\> 0, 1 <\> 1, 0 <\> 2], 
                  [2 <\> 0, 2 <\> 0, 2 <\> 0, 1 <\> 1]
                ]
         
g4Game' = mkGame [ [1 <\> 1, 2 <\> 0, 0 <\> 2],
                   [0 <\> 2, 1 <\> 1, 0 <\> 2], 
                   [2 <\> 0, 2 <\> 0, 1 <\> 1]
                 ]

g4Game'' = mkGame [ [1 <\> 1, 0 <\> 2],
                    [2 <\> 0, 1 <\> 1]
                  ]

{-
maximize u with r*1+p*2+s*0+w*2>=u,r*0+p*1+s*2+w*0>=u,r*2+p*0+s*1+w*2>=u,r*0+p*2+s*0+w*1>=u,r+p+s+w=1,r>=0,p>=0,s>=0,w>=0
-}
-- main = print $ maxiMin g2Game ColPlayer
-- main = print $ expUtility g2Game RowPlayer ([0,1/3,1/3,1/3],[0,1/3,1/3,1/3])
         
prisoner = mkGame [ [1 <\> 1, 3 <\> 0]
                  , [0 <\> 3, 2 <\> 2]]
-- main = print $ expUtility prisoner RowPlayer ([1.0,0.0],[0.0,1.0])
         
-- main = print $ maxiMin prisoner RowPlayer


{-
{-
import qualified Data.Map as Map
import Data.Map(Map)

-- Preferences are possible over all ord instances, i.e normally, preferences should simply correspond to the normal ordering implied by the standard Ord instance.
-- We get completeness as transitivity "for free" that way.

-- Utility functions must be monotonic, i.e x <= y ==> u(x) <= u(y)
type UtilityFn a r = (Ord a, Fractional r) => a -> r

type Probability = Rational

-- the probabilities in a lottery must add up to 1
type Lottery a = (Ord a) => Map a Probability
-- TODO: maybe add a custom Ord instance?

expUtility :: (Ord a, Fractional r) => Lottery a -> UtilityFn a r -> r
expUtility lottery u = Map.foldrWithKey (\x p expU -> expU + ((fromRational p) * (u x))) 0 lottery

-}

{-
Maximize U_1

where
s_1_A * 2 + s_1_B * 0 >= U_1
s_1_A * 0 + s_1_B * 1 >= U_1
s_1_A >= 0
s_1_B >= 0
s_1_A + s_1_B = 1

Note that each strategy of the opponent (a_[-i]) needs its own constraint.
... >= U_1 maps to ... - U_1 >= 0.
-}

-- variables are s_1_A, s_2_A, U_1
battleOfSexesProb = Maximize [0, 0, 1]
battleOfSexesConstr = 
  Dense [ [2, 0, -1] :=>: 0
        , [0, 1, -1] :=>: 0
        , [0, 1, 0] :=>: 0
        , [1, 1, 0] :==: 1
        ]
  
solBattleSexes = simplex battleOfSexesProb battleOfSexesConstr []

-- main = print sol
          
       
{-
G-Homework:
s_1_A * 0 + s_1_B * 1 >= U_1
s_1_A * 3 + s_1_B * 0 >= U_1
-}

hwProb = Maximize [0, 0, 1]
hwConstr = 
  Dense [ [0, 1, -1] :=>: 0
        , [3, 0, -1] :=>: 0
        , [0, 1, 0] :=>: 0
        , [1, 1, 0] :==: 1
        ]
  
solHw = simplex hwProb hwConstr []

main = print solHw


{-
Exp payoff for player 1:
s_1_A * s_2_A * 0 + s_1_A * s_2_B * 3 +
s_1_B * s_2_A * 1 + s_1_B * s_2_B * 0
= s_1_A * s_2_B * 3 + s_1_B * s_2_A * 1

similarly for player 2.

This is not a linear problem.
-}

hwProb2 = Maximize []
hw2Constr =
  Dense [ 






{-
prob = Maximize [4, -3, 2]

{-
constr1 = Sparse [ [2#1, 1#2] :<: 10
                 , [1#2, 5#3] :<: 20
                 ]
-}
constr2 = Dense [ [2,1,0] :<=: 10
                , [0,1,5] :<=: 20
                ]
          
foo = simplex prob constr2 []

main = print foo
-}

-}
-}