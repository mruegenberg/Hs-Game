{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FunctionalDependencies #-}

-- | Solution concepts for two-player games
module Main where

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

yankPos :: (NaturalNumber n) => (Pos Int (SuccessorTo n)) -> Int -> Pos Int n
yankPos (Pos l n) i = Pos (snd (yank (i - 1) l)) (predecessorOf n)

emptyPos :: Pos a N0
emptyPos = Pos [] n0
        
appendPos :: (NaturalNumber n) => (Pos a n) -> a -> Pos a (SuccessorTo n)
appendPos g@(Pos l n) v = insertPos g (length l) v
    
    
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
-- main = print $ expectedUtility g2Game RowPlayer ([0,1/3,1/3,1/3],[0,1/3,1/3,1/3])
         
prisoner = mkGame [ [1 <\> 1, 3 <\> 0]
                  , [0 <\> 3, 2 <\> 2]]
-- main = print $ expectedUtility prisoner RowPlayer ([1.0,0.0],[0.0,1.0])
         
-- main = print $ maxiMin prisoner RowPlayer


{-

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