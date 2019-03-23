{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}


module Data.Tsuro.Interface.Game
  ( start
  )
where

import Control.Monad
import Data.Char
import Data.Either
import Data.Ix
import Data.List
import Data.Tsuro
import Data.Tsuro.Interface.Util
import Data.Tsuro.Read
import Data.Tsuro.Search.Core
import qualified Data.Tsuro.Search.Montecarlo as Montecarlo
import Data.Tsuro.Show
import System.Console.Pretty
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

inputGameMove :: Game -> IO (Either Game GameMove)
inputGameMove game = do
  flushStr $ showInputString game
  input <- getLine
  case input of
    ":p" -> do
      flushStrLn $ colorMessage $ "@ Possible moves: " ++ unwords (map showRich $ possibleMoves game)
      inputGameMove game
    ':' : 's' : rest -> case parseSearch rest of
      Nothing -> do
        flushStrLn $ colorError "@ No such algorithm."
        inputGameMove game
      Just (SomeSearch search) -> do
        (move, ratio) <- runSearchWithRatio search $ fromRight undefined (gameStateOf game)
        flushStrLn $ colorMessage $ "@ Suggested move: " ++ showRich move ++ " (" ++ printf "%.2f" (ratio * 100) ++ "%)"
        inputGameMove game
    ':' : 'c' : ' ' : rest -> case parseTile rest of
      Nothing -> do
        flushStrLn $ colorError "@ Invalid input"
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

data SomeSearch = forall s. Search IO s => SomeSearch s

parseSearch :: String -> Maybe SomeSearch
parseSearch string =
  case string of
    "m" -> Just $ SomeSearch Montecarlo.defaultConfig
    "mf" -> Just $ SomeSearch (Montecarlo.Config 1000 4 3)
    _ -> Nothing

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