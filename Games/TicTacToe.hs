{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module Games.TicTacToe where

import Test.HUnit
import Data.Binary
import Data.List
import Data.List.Split

import Solver
import PlayableGame



-- | Data type for representing cell contents - Empty / X / O
data Symbol = E | X | O
deriving (Show, Eq, Ord)

type TTTBoard = [Symbol]

boardSize :: Int
boardSize = 3

data TTTState = TTTState { board  :: TTTBoard
                         , player :: Player
                         } deriving(Show, Eq, Ord)


-- | Populates and returns the initial board state
tttInitPosition :: TTTState
tttInitPosition = TTTState (replicate (boardSize * boardSize) E) PlayerOne

-- | Returns a modified board with @posn@ element set to @symb@
setSymbol :: Move -> Symbol -> TTTBoard -> TTTBoard
setSymbol posn symb cboard
            =   map modify $ zip [1..] cboard
                where modify posn E = symb
                      modify _    s = s

flipPlayer :: Player -> Player
flipPlayer PlayerOne = PlayerTwo
flipPlayer PlayerTwo = PLayerOne



tttWhoseTurn :: TTTState -> Player
tttWhoseTurn (TTTState _ player) = player


tttDoMove :: TTTState -> Move -> TTTState
tttDoMove (TTTState cboard PlayerOne) i = TTTState (setSymbol i O cboard) PlayerTwo
tttDoMove (TTTState cboard PlayerTwo) i = TTTState (setSymbol i X cboard) PlayerOne


tttGetMoves :: TTTState -> [Move]
tttGetMoves (TTTState cboard _) = map fst $ filter isEmpty (zip [1..] b)
                                  where isEmpty (_, sym) = sym == E





-- initPosition :: a
-- doMove :: a -> Move -> a
-- baseCase :: a -> Result
-- getMoves :: a -> [Move]
-- whoseTurn :: a -> Player


instance Game TTTState where
  initPosition = tttInitPosition
  doMove = tttDoMove
  baseCase = tttPrimitive
  getMoves = tttGetMoves
  whoseTurn = tttWhoseTurn

boardToString :: TTTBoard -> String
boardToString b = intercalate divider rows where
  divider = take (maxLen * boardSize + (boardSize - 1)) (repeat '-') ++ "\n"
  rows' = zip [(1 :: Int)..] b
  rows'' = map (\(n, p) -> if p == E then show n else show p) rows'
  maxLen = maximum . map length $ rows''
  rows''' = map (take maxLen . (++ repeat ' ')) rows''
  rows = map ((++ "\n") . intercalate "|") $ boardTo2D rows'''

instance PlayableGame TTTBoard where
  showBoard = boardToString
  showMoves = show

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


testPrimitive :: Test
testPrimitive = TestList $
                [Win ~=? primitive board1,
                 Tie ~=? primitive board2,
                 Lose ~=? primitive board3]