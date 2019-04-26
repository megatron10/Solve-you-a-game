{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Solver (Player(..), getNextPlayer, Move, Result(..), Game(..), GameMap) where

{-|
    TODO :
    solveGame
    getResult
-}

import qualified Data.Map as M
import Data.Map (Map)
import qualified Control.Monad.State as S

-- | The @Player@ type stores whose turn it is to play in Game
data Player = PlayerOne | PlayerTwo
              deriving (Show, Read, Eq, Ord)

{-|
    getNextPlayer takes the Player playing the current turn
    returns the Player playing in the next turn
-}
getNextPlayer :: Player -> Player
getNextPlayer PlayerOne = PlayerTwo
getNextPlayer PlayerTwo = PlayerOne


{-|
    @Move@ is a type synonym for Int
    It stores integer corresponding to a move in Game
-}
type Move = Int


-- | The @Result@ type stores the expected outcome of a game state
data Result = Undecided | Lose | Tie | Win
              deriving(Show, Read, Eq, Ord)

{-|
    Typeclass to specify games.
    Games supported by this framework should be defined as instances of the Game typeclass.
-}
class (Ord a, Show a) => Game a where
    -- | Initial Position of Game
    initPosition :: a

    {-|
        doMove takes as input the present game state, and the move to be performed
        It returns the game state after the move is made
    -}
    doMove :: a -> Move -> a

    {-|
        baseCase takes the present game state as input
        It returns the end result of the game ( Win/Lose ) if it is completed
        Otherwise it returns the Result Undecided
    -}
    baseCase :: a -> Result
    
    {-|
        getMoves takes the present Game state as input
        Returns a list of Game states that valid moves from the input state can lead to in one move
    -}
    getMoves :: a -> [Move]

    {-|
        whoseTurn takes the present Game state as input
        Returns the Player whose turn it is make a move
    -}
    whoseTurn :: a -> Player


-- | Type synonym for a map of Game states to result
type GameMap a = Map a Result


{-|
    SolverState a (here a is GameState) is an instance of the State Monad from Control.Monad.State.
    It has in its state the GameMap generated by all computations performed.
    It has been extensively used in the solveGame function described later, that performs the MiniMax algorithm.
-}
type SolverState a = S.State (GameMap a) Result


{-|
    Based on ordering(member of Ord typeclass) given to Results,
    Player One chooses the optimal move based on the results of all the child states
-}
playerOneOptimal :: [Result] -> Result
playerOneOptimal vals | null vals = error "No children to choose from, should have used primitive for base case handling"
                      | otherwise = maximum vals


{-|
    Based on ordering(member of Ord typeclass) given to Results,
    Player Two chooses the optimal move based on the results of all the child states
-}
playerTwoOptimal :: [Result] -> Result
playerTwoOptimal vals | null vals = error "No children to choose from, should have used primitive for base case handling"
                      | otherwise = minimum . filter (/=Undecided) $ vals
