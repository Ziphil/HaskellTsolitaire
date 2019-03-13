--


module Tsuro.Interface where

import Data.List
import System.Console.Pretty
import System.Random
import Tsuro.Base
import Tsuro.Read
import Tsuro.Show
import ZiphilUtil


start :: IO ()
start = do
  gen <- getStdGen
  loop $ initialGame gen

colorInput :: Pretty a => a -> a
colorInput = color Yellow

colorHand :: Pretty a => a -> a
colorHand = style Reverse . color Yellow

colorMessage :: Pretty a => a -> a
colorMessage = color Cyan

colorError :: Pretty a => a -> a
colorError = color Red

showInputString :: Game -> String
showInputString game = either (const "") show' (number <$> nextHand game)
  where
    show' = interpose (colorInput "<?> ") (colorInput " -> ") . colorHand . pad 2 . show

loop :: Game -> IO ()
loop game = do
  putStrLn $ showRec game
  case (isCleared game, isOver game) of
    (True, _) -> putStrLn $ colorMessage "@ Congratulations! You win the game."
    (False, True) -> do
      putStr $ showInputString game
      putStrLn "---"
      putStrLn $ colorMessage "@ Game over! Try again!"
    (False, False) -> do
      nextGame <- getNextGame game
      putStrLn ""
      loop nextGame

inputGameMove :: Game -> IO GameMove
inputGameMove game = do
  putStr $ showInputString game
  input <- getLine
  if input == "?"
    then do
      putStrLn $ colorMessage $ "@ Possible moves: " ++ unwords (map showRec $ possibleMoves game)
      inputGameMove game
    else case readRec input of
      Nothing -> do
        putStrLn $ colorError "@ Invalid input. Specify the position and rotation in the form like '5FR' or '1BT'."
        inputGameMove game
      Just move -> return move

getNextGame :: Game -> IO Game
getNextGame game = do
  move <- inputGameMove game
  case applyMove move game of
    Left OutOfBoard -> do
      putStrLn $ colorError "@ Some stone will go out of the board."
      getNextGame game
    Left TileAlreadyPut -> do
      putStrLn $ colorError "@ Some tile is already put there."
      getNextGame game
    Left DetachedTilePos -> do
      putStrLn $ colorError "@ The specified position is not adjacent to any stone."
      getNextGame game
    Right game -> return game