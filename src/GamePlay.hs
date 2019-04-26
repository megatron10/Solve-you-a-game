{-# LANGUAGE IncoherentInstances #-}

module GamePlay where

import           Control.Monad.State as St
import           Data.List           as List
import           Data.Map            as M
import           Prelude             as Prelude
import           Solver

{-|
  nextPositon takes games current position
  and returns list of moves , gamepositons possible.
-}
nextPosition :: PlayableGame a => a -> [Move] -> [(Move, a)]
nextPosition pos = Prelude.foldr (\m xs -> (m, doMove pos m) : xs) []

class Game a =>
      PlayableGame a
  {-|
  showGame is function that takes a game state and
  returns a string that will printed while playing game
  This function has to be provided by game implementation
  -}
  where
  showGame :: a -> String
  {-|
  printMoves is a function that takes a game state and
  returns a string that will print possible game moves
  for the human player to play.
  This function has to be provided by game implementation
  -}
  printMoves :: a -> String
  {-|
  choseBestMove is a function that takes list of moves ,
  game state and game map and calculates the move to
  take using values from gameMap.
  -}
  chooseBestMove :: [Move] -> a -> GameMap a -> Move
  chooseBestMove moves gamepos gamemap = best
    where
      func =
        \(a, possible_position) ->
          ( a
          , M.findWithDefault
              (error "Value Not found in Map")
              possible_position
              gamemap)
      nextvalues = Prelude.map func (nextPosition gamepos moves)
      (best, _) = List.minimumBy (\a b -> compare (snd a) (snd b)) nextvalues

{-|
	data GameState stores the current game state it stores
	gamemap and current game position.
-}
data GameState a = GameState
  { gamemap     :: GameMap a
  , curPosition :: a
  }

type GamePlayState a b = StateT (GameState a) IO b

{-|
 playTheGame function that takes game state and
 plays the game for one step. This function decides
 if the game is finised or not.
-}
playTheGame :: PlayableGame a => GamePlayState a ()
playTheGame = do
  gamest <- get
  let posi = curPosition gamest
  case baseCase posi of
    Win -> do
      lift $
        putStrLn "-----------------You win! ------------------ \nGame state: "
      lift $ putStrLn (showGame posi)
      return ()
    Lose -> do
      lift $
        putStrLn "-----------------Computer wins!-------------- \nGame state: "
      lift $ putStrLn (showGame posi)
      return ()
    Tie -> do
      lift $
        putStrLn "-----------------It's a tie!----------------- \nGame state:"
      lift $ putStrLn (showGame posi)
      return ()
    Undecided ->
      case whoseTurn posi of
        PlayerOne -> humanPlay
        PlayerTwo -> computerPlay

{-|
	humanPlay function this function makes one
	step of human gameplay. prints gamestate,
	avilable options and make a move according to
	The input.
-}
humanPlay :: PlayableGame a => GamePlayState a ()
humanPlay = do
  gamest <- get
  let posi = curPosition gamest
  let moves = getMoves posi
  lift $ putStr ("\ncurrent Game State :\n" ++ showGame posi)
  lift $ putStrLn ("Possible moves: " ++ show moves)
  humanmove <- lift $ getInputMove moves
  put $ GameState (gamemap gamest) (doMove posi humanmove)
  playTheGame

{-|
	computerMove function takes a gamestate of a playableGame and
	makes one step of computer move this prints move taken 
	by the computer and the final state of the game.
-}
computerPlay :: PlayableGame a => GamePlayState a ()
computerPlay = do
  gamest <- get
  let posi = curPosition gamest
  let moves = getMoves posi
  bestAvailableMove <- bestMove moves posi
  put $ GameState (gamemap gamest) (doMove posi bestAvailableMove)
  lift $ putStr ("current Game State:\n" ++ showGame posi)
  lift $ putStrLn ("Computer chose move " ++ show bestAvailableMove)
  playTheGame

{-|
	bestMove function takes list of moves and game position
	as inputs and determines the bestPossibleMove in current
	postion uses function chooseBestMove
-}
bestMove :: PlayableGame a => [Move] -> a -> GamePlayState a Move
bestMove moves posi = do
  gamest <- get
  return $ chooseBestMove moves posi (gamemap gamest)

{-|
	parseInputMove function that takes a string
	and maybe return a Move.
-}
parseInputMove :: String -> Maybe Move
parseInputMove s =
  case reads s of
    [(move, "")] -> Just move
    _            -> Nothing

{-|
	getInputMove function takes a list of moves
	and takes a Input of a  valid move from the user.
-}
getInputMove :: [Move] -> IO Move
getInputMove valid = do
  putStr "Your Move : \n"
  moveString <- getLine
  let inputmove = parseInputMove moveString
  case inputmove of
    Just m
      | m `elem` valid -> return m
    _ -> do
      putStrLn "That's not a valid move..."
      getInputMove valid
