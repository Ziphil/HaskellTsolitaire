--


module Data.Tsuro.Interface.Game where

import Data.Either
import Data.List
import Data.Tsuro
import Data.Tsuro.Interface.Core
import Data.Tsuro.Read
import Data.Tsuro.Search.Core
import qualified Data.Tsuro.Search.Montecarlo as Montecarlo
import Data.Tsuro.Show
import System.Console.Pretty
import System.IO
import System.Random
import Text.Printf
import Ziphil.Util.List


start :: IO ()
start = do
  game <- initialGame
  loop game

showInputString :: Game -> String
showInputString game = either (const "") show' (number <$> nextHand game)
  where
    show' = interpose (colorInput "<?> ") (colorInput " -> ") . colorHand . padl 2 ' ' . show

loop :: Game -> IO ()
loop game = do
  flushStrLn $ showRich game
  case (isCleared game, isOver game) of
    (True, _) -> flushStrLn $ colorMessage "@ Congratulations! You win the game."
    (False, True) -> do
      flushStr $ showInputString game
      flushStrLn "---"
      flushStrLn $ colorMessage "@ Game over! Try again!"
    (False, False) -> do
      nextGame <- getNextGame game
      flushStrLn ""
      loop nextGame

inputGameMove :: Game -> IO GameMove
inputGameMove game = do
  flushStr $ showInputString game
  input <- getLine
  case input of
    ":p" -> do
      flushStrLn $ colorMessage $ "@ Possible moves: " ++ unwords (map showRich $ possibleMoves game)
      inputGameMove game
    ":s" -> do
      (move, ratio) <- runSearchWithRatio Montecarlo.defaultConfig $ fromRight undefined (gameStateOf game)
      flushStrLn $ colorMessage $ "@ Suggested move: " ++ showRich move ++ " (" ++ printf "%.2f" (ratio * 100) ++ "%)"
      inputGameMove game
    _ -> case readRich input of
      Nothing -> do
        flushStrLn $ colorError "@ Invalid input. Specify the position and rotation in the form like '5FR' or '1BT'."
        inputGameMove game
      Just move -> return move

getNextGame :: Game -> IO Game
getNextGame game = do
  move <- inputGameMove game
  case applyMove move game of
    Left OutOfBoard -> do
      flushStrLn $ colorError "@ Some stone will go out of the board."
      getNextGame game
    Left TileAlreadyPut -> do
      flushStrLn $ colorError "@ Some tile is already put there."
      getNextGame game
    Left DetachedTilePos -> do
      flushStrLn $ colorError "@ The specified position is not adjacent to any stone."
      getNextGame game
    Right nextGame -> return nextGame