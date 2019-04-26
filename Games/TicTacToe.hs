{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module TicTacToe where

-- import Test.HUnit
-- import Data.Binary
import Data.List
import Data.List.Split

import Solver
import GamePlay

-- | Data type for representing cell contents - Empty / X / O
data Symbol = E | X | O
                    deriving (Show, Eq, Ord)

-- | Represents the current Board state
type TTTBoard = [Symbol]

-- | Flexible TicTacToe implementation.
-- | Changing the number will change size.
boardSize :: Int
boardSize = 3

-- | Main State type. Stores game state.
data TTTState = TTTState { board  :: TTTBoard   -- ^ Represents Cell contents of board
                         , player :: Player     -- ^ Which player will move next
                         } deriving(Show, Eq, Ord)


-- | Populates and returns the initial board state
tttInitPosition :: TTTState
tttInitPosition = TTTState (replicate (boardSize * boardSize) E) PlayerOne

-- | Returns a modified board with @posn@ element set to @symb@
setSymbol :: Move -> Symbol -> TTTBoard -> TTTBoard
setSymbol posn symb cboard
            =   map modify $ zip [1..] cboard
                where modify (i, s)
                                | i == posn = symb
                                | otherwise = s


-- | API function returning next Player
tttWhoseTurn :: TTTState -> Player
tttWhoseTurn = player

-- | API function, takes game state and move
-- | returns modified state after playing move
tttDoMove :: TTTState -> Move -> TTTState
tttDoMove (TTTState cboard PlayerOne) i = TTTState (setSymbol i X cboard) PlayerTwo
tttDoMove (TTTState cboard PlayerTwo) i = TTTState (setSymbol i O cboard) PlayerOne

-- | API function, returns available moves to Solver
tttGetMoves :: TTTState -> [Move]
tttGetMoves (TTTState cboard _) = map fst $ filter isEmpty (zip [1..] cboard)
                                  where isEmpty (_, sym) = sym == E

{-|
    Generating every @boardSize@ line.
    They will then be compares to a filled winner line.
-}

-- | Extracts forward diagonal of board
getDiag1 :: TTTBoard -> [Symbol]
getDiag1 b = map snd $ filter isD1 (zip [1..] b)
             where isD1 (n, _) =  n `rem` (boardSize+1) == 1


-- | Extracts backward diagonal of board
getDiag2 :: TTTBoard -> [Symbol]
getDiag2 b = map snd $ filter isD2 (zip [1..] b)
             where isD2 (n, _) =  n `rem` (boardSize-1) == 1 && n>1 && n<(boardSize * boardSize)

-- | Encloses both the diagonals in a list             
getDiags :: TTTBoard -> [[Symbol]]
getDiags b = [getDiag1 b , getDiag2 b]

-- | Extracts rows and returns a list of rows
getRows :: TTTBoard -> [[Symbol]]
getRows = chunksOf boardSize


-- | Extracts columns and returns a list of colums
getCols :: TTTBoard -> [[Symbol]]
getCols = transpose . getRows

-- | Merges all lines and combines them in a list.
-- | returns a list of lines i.e. lists of symbols.
getLines :: TTTBoard -> [[Symbol]]
getLines b = getRows b ++ getCols b ++ getDiags b


-- | API function, returns a @Result@
-- | Evaluates state and returns a Win / Lose / Tie / Nothing
tttBaseCase :: TTTState -> Result
tttBaseCase (TTTState cboard _)
                        | winLine_X `elem` tttlines = Win
                        | winLine_O `elem` tttlines = Lose
                        | E `notElem`cboard      = Tie
                        | otherwise              = Undecided
                        where winLine_X = replicate boardSize X
                              winLine_O = replicate boardSize O
                              tttlines = getLines cboard

-- | Making our game an instance of Game
-- | Adhering to the API provided
instance Game TTTState where
  initPosition = tttInitPosition
  doMove = tttDoMove
  baseCase = tttBaseCase
  getMoves = tttGetMoves
  whoseTurn = tttWhoseTurn

-- | Makes a string out of the board for text UI.
boardToString :: TTTBoard -> String
boardToString b = intercalate horizLine rows
                  where horizLine = (replicate (4*boardSize -1) '-') ++ "\n"
                        row l = (intercalate "|" $ (map s2c l)) ++ " \n"
                        rows = map row $ getRows b
                        s2c E = "   "
                        s2c X = " X "
                        s2c O = " O "


-- | Pretty Print set of available moves for user
tttPrintMoves :: TTTBoard -> String
tttPrintMoves b = show $ map fst $ filter isEmpty $ zip [1::Int ..] b
                  where isEmpty (_,s) = s == E
                        
-- | Making our game an instance of PlayableGame
-- | Adhering to the API provided                  
instance PlayableGame TTTState where
  showGame = boardToString . board
  printMoves = tttPrintMoves . board

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