module CHOMPtest
  ( testDoMove
  ) where

import           GamePlay
import           Solver
import           Test.HUnit
import           Chomp

board1 :: Chocolate
board1 = [[O, O, O, O, O, O],
          [O, O, O, O, O, O],
          [O, O, O, O, O, O],
          [O, O, O, O, O, O]]

board1out :: Chocolate
board1out = [[O, O, O, O, O, O],
             [O, O, O, O, O, O],
             [O, O, X, X, X, X],
             [O, O, X, X, X, X]]

board2 :: Chocolate
board2 = [[O, O, O, O, O, O],
          [O, O, O, O, O, O],
          [O, O, X, X, X, X],
          [O, O, X, X, X, X]]

board2out :: Chocolate
board2out = [[O, O, O, O, X, X],
             [O, O, O, O, X, X],
             [O, O, X, X, X, X],
             [O, O, X, X, X, X]]

testDoMove :: Test
testDoMove =
  TestList $
  [ (ChompState board1out PlayerTwo) ~=? doMove (ChompState board1 PlayerOne) 15
  , (ChompState board2out PlayerTwo) ~=? doMove (ChompState board2 PlayerOne) 5
  ]
