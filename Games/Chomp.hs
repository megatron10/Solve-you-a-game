{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Chomp where

import           Data.List
import           Data.List.Split

import           GamePlay
import           Solver

-- | Data type for representing cell contents - X (Taken) / O (Available)
data Symbol
  = X
  | O
  deriving (Eq, Ord)

-- | Display Symbol neatly
instance Show Symbol where
  show X = " X "
  show O = " O "

-- | Tyne synonym for storing the state of the @Chocolate@ ( or board )
type Chocolate = [[Symbol]]

-- | Flexible boardSize is used
-- We can modify just the size in the 2-tuple below and the game will be accordingly modified
-- 4x6 is just taken as a sample size
boardSize :: (Int, Int)
boardSize = (4, 6)

-- | Gives number of cells on the chocolate(board)
cells :: Int
cells = fst boardSize * snd boardSize

{-|
  @ChompState@ is a data type to store the chomp game state
  It holds a @Chocolate@ that shows state of the grid
  and a @Player@ that denotes whose turn it is to play
-}
data ChompState = ChompState
  { candy  :: Chocolate
  , player :: Player
  } deriving (Show, Eq, Ord)

-- | Populates and returns the initial @ChompState@
chompInitPosition :: ChompState
chompInitPosition =
  ChompState (replicate (fst boardSize) (replicate (snd boardSize) O)) PlayerOne

-- | Returns the @Player@ whose turn it is to play
chompWhoseTurn :: ChompState -> Player
chompWhoseTurn = player

{-|
  Takes a @ChompState@ and a @Move@ to return the updated @ChompState@
  The @ChompState@ is updated as per standard chomp rules
-}
chompDoMove :: ChompState -> Move -> ChompState
chompDoMove state move = ChompState eaten (getNextPlayer $ player state)
  where
    row v = (v + (snd boardSize) - 1) `div` (snd boardSize)
    col v = ((v - 1) `rem` (snd boardSize)) + 1
    nr = row move
    nc = col move
    taken v = ((row v) >= nr) && ((col v) >= nc)
    helper (x, l) s =
      ( x - 1
      , if (s == X || taken x)
          then X : l
          else O : l)
    eaten =
      chunksOf
        (snd boardSize)
        (snd $ foldl helper (cells, []) (reverse (intercalate [] (candy state))))

-- | Takes a @ChompState@ and returns the list of valid moves possible from the state
chompGetMoves :: ChompState -> [Move]
chompGetMoves (ChompState choco _) =
  map fst $ filter isChoco (zip [1 ..] (intercalate [] choco))
  where
    isChoco (_, sym) = sym == O

{-|
  Takes a @ChompState@ and returns the @Result@ of the game
  i.e. (Win/Tie/Lose) if it has ended or Undecided otherwise
-}
chompBaseCase :: ChompState -> Result
chompBaseCase (ChompState choco p)
  | (head (head choco) == X) =
    if p == PlayerOne
      then Win
      else Lose
  | otherwise = Undecided

-- | Makes @ChompState@ an instance of @Game@
-- representing Chomp in the framework specified
instance Game ChompState where
  initPosition = chompInitPosition
  doMove = chompDoMove
  baseCase = chompBaseCase
  getMoves = chompGetMoves
  whoseTurn = chompWhoseTurn

-- | Takes a @Chocolate@ and returns a pretty @String@ for printing
chocoString :: Chocolate -> String
chocoString b = intercalate horizLine rows
  where
    horizLine = (replicate (4 * (snd boardSize) - 1) '-') ++ "\n"
    row l = (intercalate "|" $ (map show l)) ++ " \n"
    rows = map row $ b

-- | Takes a @Chocolate@ and returns a string representing valid moves
chompPrintMoves :: Chocolate -> String
chompPrintMoves b =
  show $ map fst $ filter isEmpty $ zip [1 ..] (intercalate [] b)
  where
    isEmpty (_, s) = s == O

-- | Making @ChompState@ an instance of @PlayableGame@
--  adhering to the API
instance PlayableGame ChompState where
  showGame = chocoString . candy
  printMoves = chompPrintMoves . candy
