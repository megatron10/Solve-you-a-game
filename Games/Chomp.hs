{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module Chomp where
import Data.List
import Data.List.Split

import Solver
import GamePlay



-- | Data type for representing cell contents - X (Taken) / O (Available)
data Symbol = X | O
                    deriving (Eq, Ord)

instance Show Symbol where
  show X = " X "
  show O = " O "

type Chocolate = [[Symbol]]

boardSize :: (Int,Int)
boardSize = (4,6)

cells = fst boardSize * snd boardSize

data ChompState = ChompState { candy  :: Chocolate
                             , player :: Player
                             } deriving(Show, Eq, Ord)


-- | Populates and returns the initial board state
chompInitPosition :: ChompState
chompInitPosition = ChompState (replicate (fst boardSize) (replicate (snd boardSize) O)) PlayerOne

chompWhoseTurn :: ChompState -> Player
chompWhoseTurn = player


chompDoMove :: ChompState -> Move -> ChompState
chompDoMove state move = ChompState eaten (getNextPlayer $ player state) where
  row v = (v+3) `div` (snd boardSize)
  col v = ((v-1) `rem` (snd boardSize)) + 1
  nr = row move
  nc = col move
  taken v = ((row v) >= nr) && ((col v) >= nc)
  helper (x,l) s = (x-1, if ( s == X || taken x ) then X:l else O:l )
  eaten = chunksOf (snd boardSize) (snd $ foldl helper (cells,[]) (reverse (intercalate [] (candy state))))


chompGetMoves :: ChompState -> [Move]
chompGetMoves (ChompState choco _) = map fst $ filter isChoco (zip [1..] (intercalate [] choco))
                                  where isChoco (_, sym) = sym == O


chompBaseCase :: ChompState -> Result
chompBaseCase (ChompState choco p)
                        | (head (head choco) == X) = if p==PlayerOne then Win else Lose
                        | otherwise                = Undecided

instance Game ChompState where
  initPosition = chompInitPosition
  doMove = chompDoMove
  baseCase = chompBaseCase
  getMoves = chompGetMoves
  whoseTurn = chompWhoseTurn

-- boardTo2D = getRows

-- boardToString :: TTTBoard -> String
-- boardToString b = intercalate divider rows where
--   divider = take (maxLen * boardSize + (boardSize - 1)) (repeat '-') ++ "\n"
--   rows' = zip [(1 :: Int)..] b
--   rows'' = map (\(n, p) -> if p == E then show n else show p) rows'
--   maxLen = maximum . map length $ rows''
--   rows''' = map (take maxLen . (++ repeat ' ')) rows''
--   rows = map ((++ "\n") . intercalate "|") $ boardTo2D rows'''




chocoString :: Chocolate -> String
chocoString b = intercalate horizLine rows
                  where horizLine = (replicate (4*(snd boardSize) -1) '-') ++ "\n"
                        row l = (intercalate "|" $ (map show l)) ++ " \n"
                        rows = map row $ b
                        

chompPrintMoves :: Chocolate -> String
chompPrintMoves b = show $ map fst $ filter isEmpty $ zip [1..] (intercalate [] b)
                  where isEmpty (_,s) = s == O
                        
instance PlayableGame ChompState where
  showGame = chocoString . candy
  printMoves = chompPrintMoves . candy

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