--


module Tsuro.Interface where

import Tsuro.Base
import Tsuro.Read
import Tsuro.Show
import System.Random
import System.Console.Pretty
import ZiphilUtil


start :: IO ()
start = do
  gen <- getStdGen
  loop $ initialGame gen

showInputString :: Game -> String
showInputString game = either (const "") show' (number <$> nextHand game)
  where
    show' = interpose "<?> " " -> " . show

loop :: Game -> IO ()
loop game = do
  putStrLn $ showRec game
  case (isCleared game, isOver game) of
    (True, _) -> putStrLn $ color Magenta $ "@ Congratulations! You win the game."
    (False, True) -> do
      putStr $ color Yellow $ showInputString game
      putStrLn "---"
      putStrLn $ color Magenta $ "@ Game over! Try again!"
    (False, False) -> do
      nextGame <- getNextGame game
      putStrLn ""
      loop nextGame

inputGameMove :: Game -> IO GameMove
inputGameMove game = do
  putStr $ color Yellow $ showInputString game
  input <- getLine
  case readRec input of
    Nothing -> do
      putStrLn $ color Red $ "@ Invalid input. Specify the position and rotation in the form like '5FR' or '1BT'."
      inputGameMove game
    Just move -> return move

getNextGame :: Game -> IO Game
getNextGame game = do
  move <- inputGameMove game
  case applyMove move game of
    Left OutOfBoard -> do
      putStrLn $ color Red $ "@ Some stone will go out of the board."
      getNextGame game
    Left TileAlreadyPut -> do
      putStrLn $ color Red $ "@ Some tile is already put there."
      getNextGame game
    Left DetachedTilePos -> do
      putStrLn $ color Red $ "@ The specified position is not adjacent to any stone."
      getNextGame game
    Right game -> return game