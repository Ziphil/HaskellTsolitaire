{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}


module Data.Tsuro.Interface.Game
  ( start
  )
where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Char
import Data.Either
import Data.Ix
import Data.List
import Data.Tsuro.Core
import Data.Tsuro.Interface.Read
import Data.Tsuro.Interface.Show
import Data.Tsuro.Interface.Util
import Data.Tsuro.Search
import System.IO
import System.Random
import Text.Printf
import Text.Read
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

pattern Prefix :: String -> String -> String
pattern Prefix mode rest <- (take 2 &&& (head . drop 2) &&& drop 3 -> (mode, (' ', rest)))

inputGameMove :: Game -> IO (Either Game GameMove)
inputGameMove game = do
  flushStr $ showInputString game
  input <- getLine
  case input of
    ":p" -> do
      flushStrLn $ colorMessage $ "@ Possible moves: " ++ unwords (map showRich $ possibleMoves game)
      inputGameMove game
    Prefix ":s" rest -> case parseSearch rest of
      Nothing -> do
        flushStrLn $ colorError "@ No such algorithm."
        inputGameMove game
      Just (SomeSearch search) -> do
        (move, ratio) <- runSearchWithRatio search $ fromRight undefined (gameStateOf game)
        flushStrLn $ colorMessage $ "@ Suggested move: " ++ showRich move ++ " (" ++ printf "%.2f" (ratio * 100) ++ "%)"
        inputGameMove game
    Prefix ":c" rest -> case parseTile rest of
      Nothing -> do
        flushStrLn $ colorError "@ Invalid input."
        inputGameMove game
      Just tile -> case changeNextHand tile game of
        Left TileNotRemaining -> do
          flushStrLn $ colorError "@ This tile is already put in the board."
          inputGameMove game
        Right nextGame -> return $ Left nextGame
    _ -> case readRich input of
      Nothing -> do
        flushStrLn $ colorError "@ Invalid input. Specify the position and rotation in the form like '5FR' or '1BT'."
        inputGameMove game
      Just move -> return $ Right move

parseTile :: String -> Maybe Tile
parseTile string = make =<< readMaybe string
  where
    make number = guard (inRange (0, tileSize - 1) number) >> Just (Tile number None)

getNextGame :: Game -> IO Game
getNextGame game = do
  result <- inputGameMove game
  case result of
    Left nextGame -> return nextGame
    Right move -> case applyMove move game of
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