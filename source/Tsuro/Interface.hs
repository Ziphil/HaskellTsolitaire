--


module Tsuro.Interface where

import Tsuro.Base
import Tsuro.Read
import Tsuro.Show
import System.Random


start :: IO ()
start = do
  gen <- getStdGen
  loop $ initialGame gen

loop :: Game -> IO ()
loop game = do
  putStrLn $ showRec game
  case (isCleared game, isOver game) of
    (True, _) -> putStrLn "Congratulations! You win the game."
    (False, True) -> do
      putStrLn $ either (const "") (\s -> "<?> " ++ showRec s ++ " -> ---") (nextHand game)
      putStrLn "Game over! Try again!"
    (False, False) -> do
      nextGame <- getNextGame game
      loop nextGame

inputGameMove :: Game -> IO GameMove
inputGameMove game = do
  putStr $ either (const "") (\s -> "<?> " ++ showRec s ++ " -> ") (nextHand game)
  input <- getLine
  case readRec input of
    Nothing -> putStrLn "! Invalid input." >> inputGameMove game
    Just move -> return move

getNextGame :: Game -> IO Game
getNextGame game = do
  move <- inputGameMove game
  case applyMove move game of
    Left OutOfBoard -> putStrLn "! Some stone will go out of the board." >> getNextGame game
    Left TileAlreadyPut -> putStrLn "! Some tile is already put there." >> getNextGame game
    Left DetachedTilePos -> putStrLn "! The specified position is not adjacent to any stone." >> getNextGame game
    Right game -> return game