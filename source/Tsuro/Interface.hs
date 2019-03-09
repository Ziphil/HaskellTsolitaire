--


module Tsuro.Interface where

import Tsuro.Base
import Tsuro.Read
import Tsuro.Show
import System.Random


start :: IO ()
start = do
  gen <- getStdGen
  turn $ initialGame gen

turn :: Game -> IO ()
turn game = do
  putStrLn $ showRec game
  nextGame <- getNextGame game
  case (isCleared nextGame, isOver nextGame) of
    (True, _) -> putStrLn "Congratulations! You win the game."
    (False, True) -> putStrLn "Game over! Try again!"
    (False, False) -> turn nextGame

inputGameMove :: IO GameMove
inputGameMove = do
  input <- getLine
  case readRec input of
    Nothing -> putStrLn "Invalid input." >> inputGameMove
    Just move -> return move

getNextGame :: Game -> IO Game
getNextGame game = do
  move <- inputGameMove
  case applyMove move game of
    Left OutOfBoard -> putStrLn "Some stone will go out of the board." >> getNextGame game
    Left TileAlreadyPut -> putStrLn "Some tile is already put there." >> getNextGame game
    Left DetachedTilePos -> putStrLn "The specified position is not adjacent to any stone." >> getNextGame game
    Right game -> return game