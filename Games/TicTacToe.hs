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
                where modify (i, s)
                                | i == posn = symb
                                | otherwise = s

-- flipPlayer :: Player -> Player
-- flipPlayer PlayerOne = PlayerTwo
-- flipPlayer PlayerTwo = PlayerOne



tttWhoseTurn :: TTTState -> Player
tttWhoseTurn = player


tttDoMove :: TTTState -> Move -> TTTState
tttDoMove (TTTState cboard PlayerOne) i = TTTState (setSymbol i X cboard) PlayerTwo
tttDoMove (TTTState cboard PlayerTwo) i = TTTState (setSymbol i O cboard) PlayerOne


tttGetMoves :: TTTState -> [Move]
tttGetMoves (TTTState cboard _) = map fst $ filter isEmpty (zip [1..] cboard)
                                  where isEmpty (_, sym) = sym == E


-- getDiag1 :: TTTBoard -> [Symbol]
-- getDiag1 b = getDs (zip [1..] b)
--              where getDs []         = []
--                    getDs ((n,s):xs) = 
--                                 | n `rem` (boardSize+1) == 1  =  s : getDs xs
--                                 | otherwise                   =  getDs xs

-- getDiag2 :: TTTBoard -> [Symbol]
-- getDiag2 b = getDs (zip [1..] b)
--              where getDs []         = []
--                    getDs ((n,s):xs) = 
--                                 | n `rem` (boardSize-1) == 1  =  s : getDs xs
--                                 | otherwise                   =  getDs xs


getDiag1 :: TTTBoard -> [Symbol]
getDiag1 b = map snd $ filter isD1 (zip [1..] b)
             where isD1 (n, _) =  n `rem` (boardSize+1) == 1

getDiag2 :: TTTBoard -> [Symbol]
getDiag2 b = map snd $ filter isD2 (zip [1..] b)
             where isD2 (n, _) =  n `rem` (boardSize-1) == 1

getDiags :: TTTBoard -> [[Symbol]]
getDiags b = [getDiag1 b , getDiag2 b]

getRows :: TTTBoard -> [[Symbol]]
getRows = chunksOf boardSize

getCols :: TTTBoard -> [[Symbol]]
getCols = transpose . getRows

getLines :: TTTBoard -> [[Symbol]]
getLines b = getRows b ++ getCols b ++ getDiags b

tttBaseCase :: TTTState -> Result
tttBaseCase (TTTState cboard _)
                        | winLine_X `elem` tttlines = Win
                        | winLine_O `elem` tttlines = Lose
                        | E `notElem`cboard      = Tie
                        | otherwise              = Undecided
                        where winLine_X = replicate boardSize X
                              winLine_O = replicate boardSize O
                              tttlines = getLines cboard


instance Game TTTState where
  initPosition = tttInitPosition
  doMove = tttDoMove
  baseCase = tttBaseCase
  getMoves = tttGetMoves
  whoseTurn = tttWhoseTurn

-- boardTo2D = getRows

-- boardToString :: TTTBoard -> String
-- boardToString b = intercalate divider rows where
--   divider = take (maxLen * boardSize + (boardSize - 1)) (repeat '-') ++ "\n"
--   rows' = zip [(1 :: Int)..] b
--   rows'' = map (\(n, p) -> if p == E then show n else show p) rows'
--   maxLen = maximum . map length $ rows''
--   rows''' = map (take maxLen . (++ repeat ' ')) rows''
--   rows = map ((++ "\n") . intercalate "|") $ boardTo2D rows'''

boardToString :: TTTBoard -> String
boardToString b = concat $ map show b


instance PlayableGame TTTState where
  showGame = boardToString . board
  printMoves = show . board

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