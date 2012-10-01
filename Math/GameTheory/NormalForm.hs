-- | Representation and algorithms for normal form / strategic form games

-- TODO: don't export the constructor for games! force people to use the <\> construct

module Math.GameTheory.NormalForm (
  -- * Definitions
    module Math.GameTheory.Common
  , Game,Player(),Action,Strategy
  , dims -- FIXME: maybe don't export?
  , utility,playerUtility
  , expectedUtility
  -- * Constructing games
  , mkGame2
  , mkGame3
  -- * Algorithms
  -- ** Maximin
  , maxiMin
  -- ** Dominance
  , dominated
  , iteratedDominance
  -- ** Stackelberg games  
  -- $stackelberg
  , stackelbergMixedCommitment
  -- ** Nash Equilibrium
  )
  where

import Math.GameTheory.Internal.NormalForm
import Math.GameTheory.Common
import TypeLevel.NaturalNumber
import Data.Array
import Numeric.LinearProgramming

---------------------- Definitions ----------------------

-- | A player. Should not be 0, and not exceed the number of players in a game where it is used.
type Player = Int

-- | A specific action for some player
type Action = Int

-- | A mixed strategy. Values in the strategy should sum up to 1, and have length equal to the number of actions in the game for the corresponding player
type Strategy = [Double]

-- | Dimensions (size) of a game.
dims :: (NaturalNumber n, Ord n) => Game n -> Pos Int n
dims (Game arr) = snd $ bounds arr

-- | The utility for all players for a specific position in the game
utility :: (NaturalNumber n, Ord n) => Game n -> Pos Action n -> Pos Double n
utility (Game arr) pos = arr ! pos

-- | Extract the utility for a specific player from the utilities for all player at a position
playerUtility :: (NaturalNumber n) => Pos Double n -> Player -> Double
playerUtility (Pos l _) p = l !! (p - 1)

-- a default position. for use with replaceAt
defaultPos :: (NaturalNumber n, Ord n) => Game n -> (Pos Int n)
defaultPos game = dims game

replacePos ::  (NaturalNumber n) => (Pos Int n) -> Int -> Int -> (Pos Int n)
replacePos (Pos l n) i v = Pos (replaceAt v l i) n

replaceAt :: a -> [a] -> Int -> [a]
replaceAt _ [] _ = []
replaceAt y (_:xs) 0 = y:xs
replaceAt y (x:xs) i = x : (replaceAt y xs (i - 1))

-- | Expected utility of some player for a strategy profile of all players
expectedUtility :: (NaturalNumber n, Ord n) => Game n -> Player -> Pos Strategy n -> Double
expectedUtility g player (Pos strategies _) = go 0 (defaultPos g) strategies
  where -- go :: (NaturalNumber n, Ord n) => Pos Int n -> [t] -> Double
        go _ pos [] = playerUtility (utility g pos) player
        go j pos (x:xs) = sum $ map (\(i,y) -> y * (go (j + 1) (replacePos pos j i) xs)) (zip [1..] x)
        
        
---------------------- Constructing Games ----------------------
        
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
          
-- TODO: n-player game
-- mkGame :: (NaturalNumber n) => n -> [] -> Game n
          
          
          
---------------------- Algorithms ----------------------

-- | Computes a maximin strategy for a player and returns it together with the security level that can be achieved with it.
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
          (Pos ds n') -> case yank (player - 1) ds of (a,b) -> (a,b,n')
          
        constraint pos' = ((-1) : map (\ownIdx -> playerUtility 
                                                 (utility game (insertPos pos' player ownIdx)) player)
                           [1..ownD])
                          :=>: 0
        otherDs = map (\d -> Pos d (predecessorOf n)) $ go $ map (\i -> [1..i]) otherDs'
        go [] = [[]]
        go (x:xs) = concatMap (\y -> map (\z -> z : y) x) (go xs)                                                 
        zeros     = repeat 0
        
----------- Dominance -----------
-- | Checks if an action of a player is dominated in the game
dominated :: (NaturalNumber n, Ord n) => Game (SuccessorTo n) -> Player -> Action -> Bool
dominated game player action = val < 1
  where (val, _) = case simplex problem constraints [] of
          Optimal r -> r
          _ -> undefined
          
        -- variables in the LP are s_i(b_i), and their sum should be minimized
        problem = Minimize (take ownD $ repeat 1)
        
        constraints = Dense $
                      (map (nonzero ownD) [1..ownD]) ++ -- s_i(b_i) >= 0 forall b_i
                      (map constraint otherDs)
                      
        nonzero d i = ((take i zeros) ++ (1 : (take (d - i) zeros))) :=>: 0
        zeros       = repeat 0
                      
        constraint pos' = (map (\ownIdx -> playerUtility (utility game (otherPos ownIdx)) player) [1..ownD])
                          :=>: (playerUtility (utility game pos) player)
            where 
              pos = insertPos pos' player action
              otherPos j = insertPos pos' player j
                      
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
    
-- TODO: change the game type so that it makes sense to return the game with the dominated actions eliminated as well
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
        
-- TODO: iteratedWeakDominance, with note that efficiency is not guaranteed
        
----------- Stackelberg games -----------

-- $stackelberg
-- In Stackelberg games, one player (the leader) may commit to a strategy before the game, and the other player (the follower) plays its action based on that information

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
    
    leaderUtil utils = playerUtility utils leader
    followerUtil utils = playerUtility utils follower
    
    
    
----------- Nash Equilibrium -----------


    
-- | Find all pure strategy Nash equilibria
pureNash :: (NaturalNumber n, Ord n) => Game n -> Pos Action n
pureNash _ = undefined

-- http://oyc.yale.edu/sites/default/files/mixed_strategies_handout_0.pdf

-- based on support enumeration. 
-- parallel algorithm for n player game: http://www.cs.wayne.edu/~dgrosu/pub/cse09.pdf
-- also need to handle degenerate games?
-- check and handle special cases for zero sum games (NEs are computable faster)

-- | Find all mixed-strategy nash equilibria
mixedNash :: (NaturalNumber n, Ord n) => Game n -> Pos Strategy n
mixedNash _ = undefined
