{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module FrogPuzzle where

import Data.List

import Solver
import GamePlay

-- | Data type for representing cell contents - Empty / X / O
data Symbol = E | R | L
                    deriving (Show, Eq, Ord)

-- | Represents the current Board state
type FPBoard = [Symbol]

-- | Flexible TicTacToe implementation.
-- | Changing the number will change size.
frogNum :: Int
frogNum = 3

-- | Main State type. Stores game state.
data FPState = FPState  { board  :: FPBoard   -- ^ Represents Cell contents of board
                        , player :: Player     -- ^ Which player will move next
                        } deriving(Show, Eq, Ord)


-- | Populates and returns the initial board state
fpInitPosition :: FPState
fpInitPosition = FPState ( (replicate frogNum R) ++ [E] ++ (replicate frogNum L) ) PlayerOne

-- | API function returning next Player
fpWhoseTurn :: FPState -> Player
fpWhoseTurn = player

-- | API function, takes game state and move
-- | returns modified state after playing move
fpDoMove :: FPState -> Move -> FPState
fpDoMove (FPState cboard PlayerOne) i = FPState cboard PlayerTwo
fpDoMove (FPState cboard PlayerTwo) i = FPState (map modify $ zip [1..] cboard) PlayerOne
                                        where modify (j, s)
                                                    | s == E    = cboard !! (i-1)
                                                    | j == i    = E
                                                    | otherwise = s

-- | API function, returns available moves to Solver
fpGetMoves :: FPState -> [Move]
fpGetMoves (FPState cboard PlayerOne) = [0]
fpGetMoves (FPState cboard PlayerTwo) = map fst $ filter isvalid $ zip [1..] cboard
                                        where (Just ii) = E `elemIndex` cboard
                                              i = ii + 1
                                              isvalid x = x `elem` [(i-1,R) , (i-2,R), (i+1,L), (i+2,L)]


                                              -- | p == PlayerOne        = Undecided

-- | API function, returns a @Result@
-- | Evaluates state and returns a Win / Lose / Tie / Nothing
fpBaseCase :: FPState -> Result
fpBaseCase st@(FPState cboard p)
                        | cboard == winnerBoard = Lose
                        | fpGetMoves st == []   = Win
                        | otherwise             = Undecided
                        where winnerBoard = (replicate frogNum L) ++ [E] ++ (replicate frogNum R)

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
boardToString b = (++"\n") $ intercalate " " $ map s2c b
                  where s2c R = ">"
                        s2c L = "<"
                        s2c E = "_"


-- | Pretty Print set of available moves for user
fpPrintMoves :: FPState -> String
fpPrintMoves st = show $ fpGetMoves st
                --   where isEmpty (_,s) = s == E
                        
-- | Making our game an instance of PlayableGame
-- | Adhering to the API provided                  
instance PlayableGame FPState where
  showGame = boardToString . board
  printMoves = fpPrintMoves

-- board1 :: TTTBoard
-- board1 = concat [[X, X, X],
--                  [O, O, X],
--                  [X, O, O]]

-- board2 :: TTTBoard
-- board2 = concat [[X, X, O],
--                  [O, O, X],
--                  [X, O, O]]

-- board3 :: TTTBoard
-- board3 = concat [[O, O, O],
--                  [O, X, X],
--                  [X, X, E]]


-- testPrimitive :: Test
-- testPrimitive = TestList $
--                 [Win ~=? primitive board1,
--                  Tie ~=? primitive board2,
--                  Lose ~=? primitive board3]