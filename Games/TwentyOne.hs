{-# OPTIONS -Wall #-}

module TwentyOne where

import Solver
import GamePlay

{-|
	Data type for oneTwoTen game holds whose turn is
	it and current sum value.
-}
data TwentyOne = TwentyOne {  turn :: Player,
                              ottvalue :: Move
                           }  deriving (Show, Read, Eq, Ord)

{-| 
	Max value to reach Aim of the game
-}
gameSize :: Move
gameSize = 21

{-| 
	TwentyOne game an instance of Game class.
-}
instance Game TwentyOne where
  initPosition = TwentyOne PlayerOne 0
  doMove b m = TwentyOne (getNextPlayer . turn $ b) (m + (ottvalue b))
  baseCase = ottbaseCase
  getMoves _ = [1, 2, 3]
  whoseTurn = turn

{-| 
	TwentyOne is a also an instance of Playable Game
-}
instance PlayableGame TwentyOne where
  showGame b = "Current: " ++ show (ottvalue b)
                ++ " Goal: " ++ show gameSize ++ "\n"
  printMoves = show 

{-| 
	Checks for result of the game and return 
	win | lose | Undecided
-}
ottbaseCase :: TwentyOne -> Result
ottbaseCase b
  | ottvalue b < gameSize = Undecided
  | turn b == PlayerOne = Win
  | otherwise = Lose 
