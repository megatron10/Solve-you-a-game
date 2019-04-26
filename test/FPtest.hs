module FPtest
  ( testBaseCase
  , testInitPosn
  ) where

import           FrogPuzzle
import           GamePlay
import           Solver
import           Test.HUnit

boardLose :: FPBoard
boardLose = [L, L, L, E, R, R, R]

boardWin :: FPBoard
boardWin = [E, R, R, R, L, L, L]

boardinit :: FPBoard
boardinit = [R, R, R, E, L, L, L]

initState :: FPState
initState = FPState boardinit PlayerOne

testInitPosn :: Test
testInitPosn = TestList $ [initState ~=? initPosition]

testBaseCase :: Test
testBaseCase =
  TestList $
  [ Win ~=? baseCase (FPState boardWin PlayerTwo)
  , Lose ~=? baseCase (FPState boardLose PlayerTwo)
  ]
