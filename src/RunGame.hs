{-# OPTIONS_GHC -Wall #-}
module RunGame where

import Solver
import GamePlay

--import Control.Monad.State.Lazy
--import Data.Map as M

{-|
	Game initialization function asks the user
	which game to  play.
-}
startGame :: IO ()
startGame = do
  putStrLn "which Game would you like to play?"
  putStr $ unlines ["1) One two Ten",
                    "2) Tic Tac Toe"]
  option <- getLine
  let opt = parseOption option
  case opt of
    {-Just 1 -> do
      t <- solveGameMap "oneTwoThe_map" :: IO (GameMap OTTBoard)
      evalStateT playGame $ GameState t initialPosition
    Just 2 -> do
      t <- solveGameMap "ticTacToe_map" :: IO (GameMap TTTBoard)
      evalStateT playGame $ GameState t initialPosition -}
    _ -> do { putStrLn "Unrecognized option"; startGame }

{-|
	parseOption takes the readed line and try convert it to
	a integer and hence return Maybe Int.
-}
parseOption :: String -> Maybe Int
parseOption string = case reads string of
  [(a,"")] -> Just a
  _ -> Nothing

                                       
runGame :: IO ()
runGame = do { startGame; runGame} 
