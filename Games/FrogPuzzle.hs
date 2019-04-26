{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}

module FrogPuzzle where

import           Data.List

import           GamePlay
import           Solver

-- | Data type for representing cell contents - Empty / X / O
data Symbol
  = E
  | R
  | L
  deriving (Show, Eq, Ord)

-- | Represents the current Board state
type FPBoard = [Symbol]

-- | Flexible Frog Puzzle implementation.
-- | Changing the number will change size.
frogNum :: Int
frogNum = 3

-- | Main State type. Stores game state.
data FPState = FPState
  { board  :: FPBoard -- ^ Represents Cell contents of board
  , player :: Player -- ^ Which player will move next
  } deriving (Show, Eq, Ord)

-- | Populates and returns the initial board state
fpInitPosition :: FPState
fpInitPosition =
  FPState ((replicate frogNum R) ++ [E] ++ (replicate frogNum L)) PlayerOne

-- | API function returning next Player
fpWhoseTurn :: FPState -> Player
fpWhoseTurn = player

-- | API function, takes game state and move
-- | returns modified state after playing move
fpDoMove :: FPState -> Move -> FPState
fpDoMove (FPState cboard PlayerOne) _ = FPState cboard PlayerTwo
fpDoMove (FPState cboard PlayerTwo) i =
  FPState (map modify $ zip [1 ..] cboard) PlayerOne
  where
    modify (j, s)
      | s == E = cboard !! (i - 1)
      | j == i = E
      | otherwise = s

-- | API function, returns available moves to Solver
fpGetMoves :: FPState -> [Move]
fpGetMoves (FPState _ PlayerOne) = [0]
fpGetMoves (FPState cboard PlayerTwo) =
  map fst $ filter isvalid $ zip [1 ..] cboard
  where
    (Just ii) = E `elemIndex` cboard
    i = ii + 1
    isvalid x = x `elem` [(i - 1, R), (i - 2, R), (i + 1, L), (i + 2, L)]

-- | API function, returns a @Result@
-- | Evaluates state and returns a Win / Lose / Nothing
-- | There are no ties here.
fpBaseCase :: FPState -> Result
fpBaseCase st@(FPState cboard _)
  | cboard == winnerBoard = Lose
  | fpGetMoves st == [] = Win
  | otherwise = Undecided
  where
    winnerBoard = (replicate frogNum L) ++ [E] ++ (replicate frogNum R)

-- | Making our game an instance of Game
-- | Adhering to the API provided
instance Game FPState where
  initPosition = fpInitPosition
  doMove = fpDoMove
  baseCase = fpBaseCase
  getMoves = fpGetMoves
  whoseTurn = fpWhoseTurn

-- | Makes a string out of the board for text UI.
boardToString :: FPBoard -> String
boardToString b = (++ "\n") $ intercalate " " $ map s2c b
  where
    s2c R = ">"
    s2c L = "<"
    s2c E = "_"

-- | Pretty Print set of available moves for user
fpPrintMoves :: FPState -> String
fpPrintMoves st = show $ fpGetMoves st

-- | Making our game an instance of PlayableGame
-- | Adhering to the API provided
instance PlayableGame FPState where
  showGame = boardToString . board
  printMoves = fpPrintMoves