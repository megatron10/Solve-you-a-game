{-# OPTIONS -Wall #-}

module OneTwoTen where

import Solver
import GamePlay

{-|
	Data type for oneTwoTen game holds whose turn is
	it and current sum value.
-}
data OneTwoTen = OneTwoTen {  turn :: Player,
                              ottvalue :: Move
                           }  deriving (Show, Read, Eq, Ord)

{-| 
	Max value to reach Aim of the game
-}
gameSize :: Move
gameSize = 10

{-| 
	OneTwoTen game an instance of Game class.
-}
instance Game OneTwoTen where
  initPosition = OneTwoTen PlayerOne 0
  doMove b m = OneTwoTen (getNextPlayer . turn $ b) (m + (ottvalue b))
  baseCase = ottbaseCase
  getMoves _ = [1, 2]
  whoseTurn = turn

{-| 
	OneTwoTen is a also an instance of Playable Game
-}
instance PlayableGame OneTwoTen where
  showGame b = "Current: " ++ show (ottvalue b)
                ++ " Goal: " ++ show gameSize ++ "\n"
  printMoves = show 

{-| 
	Checks for result of the game and return 
	win | lose | Undecided
-}
ottbaseCase :: OneTwoTen -> Result
ottbaseCase b
  | ottvalue b < gameSize = Undecided
  | turn b == PlayerOne = Lose
  | otherwise = Win 
