{-# OPTIONS_GHC -Wall #-}

module RunGame where

import           Control.Monad.State.Lazy
import           FrogPuzzle
import           GamePlay
import           Solver
import           TicTacToe
import           TwentyOne

--import Data.Map as M
{-|
	Game initialization function This asks
	user which game to play currently provided
	example games are The Twenty one game and
	Tic Tac Toe.
-}
startGame :: IO ()
startGame = do
  putStrLn "which Game would you like to play?"
  putStr $
    unlines ["1) The Twenty One Game", "2) Tic Tac Toe", "3) Frog Puzzle"]
  option <- getLine
  let opt = parseOption option
  case opt of
    Just 1 -> do
      t <- solveGameMap "oneTwoTen" :: IO (GameMap TwentyOne)
      evalStateT playTheGame $ GameState t initPosition
    Just 2 -> do
      t <- solveGameMap "ticTacToe" :: IO (GameMap TTTState)
      evalStateT playTheGame $ GameState t initPosition
    Just 3 -> do
      t <- solveGameMap "frogPuzzle" :: IO (GameMap FPState)
      evalStateT playTheGame $ GameState t initPosition
    _ -> do
      putStrLn "Unrecognized option"
      startGame

{-|
	parseOption takes the readed line and try convert it to
	a integer and hence return Maybe Int.
-}
parseOption :: String -> Maybe Int
parseOption string =
  case reads string of
    [(a, "")] -> Just a
    _         -> Nothing

runGame :: IO ()
runGame = do
  startGame

solveGameMap :: (Game a) => FilePath -> IO (GameMap a)
solveGameMap fname = do
  return solveGame
