--

module Data.Tsolitaire.Interface.Simulate
  ( start
  )
where

import Data.Tsolitaire.Core
import Data.Tsolitaire.Interface.Show
import Data.Tsolitaire.Interface.Util
import Data.Tsolitaire.Search
import Data.ZiphilUtil.Core


start :: IO ()
start = do
  SomeSearch search <- inputSearch
  flushStrLn ""
  prepareProgress
  ((game, record), status) <- simulateWithHook updateProgress search
  flushStrLn ""
  flushStrLn $ showRich game
  case status of
    Success -> do
      flushStrLn $ colorMessage "@ Succeeded."
      flushStrLn $ colorMessage $ "@ Record: " ++ showRich record
    Failure -> do
      flushStrLn $ colorMessage "@ Failed."
      flushStrLn $ colorMessage $ "@ Record: " ++ showRich record ++ " " ++ either (const "") (show . number) (nextHand game) ++ "/"

inputSearch :: IO SomeSearch
inputSearch = do
  flushStr $ colorInput "<?> Algorithm -> "
  input <- getLine
  case parseSearch input of
    Just search -> return search
    Nothing -> do
      flushStrLn $ colorError "@ No such algorithm."
      inputSearch