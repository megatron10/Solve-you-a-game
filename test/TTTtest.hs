module TTTtest(testBaseCase, testInitPosn) where

import TicTacToe
import Test.HUnit
import GamePlay
import Solver

board1 :: TTTBoard
board1 = concat [[X, X, X],
                 [O, O, X],
                 [X, O, O]]

board2 :: TTTBoard
board2 = concat [[X, X, O],
                 [O, O, X],
                 [X, O, O]]

board3 :: TTTBoard
board3 = concat [[O, O, O],
                 [O, X, X],
                 [X, X, E]]

                 
boardinit :: TTTBoard
boardinit = concat [[E, E, E],
                    [E, E, E],
                    [E, E, E]]

initState :: TTTState
initState = TTTState boardinit PlayerOne

testInitPosn :: Test
testInitPosn = TestList $ [initState ~=? initPosition]

testBaseCase :: Test
testBaseCase = TestList $
                [Win  ~=? baseCase (TTTState board1 PlayerOne) ,
                 Tie  ~=? baseCase (TTTState board2 PlayerOne) ,
                 Lose ~=? baseCase (TTTState board3 PlayerOne) ]
