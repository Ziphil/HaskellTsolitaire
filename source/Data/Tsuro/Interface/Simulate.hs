--


module Data.Tsuro.Interface.Simulate
  ( start
  )
where

import Data.Tsuro.Core
import Data.Tsuro.Interface.Show
import Data.Tsuro.Interface.Util
import Data.Tsuro.Search


start :: IO ()
start = do
  SomeSearch search <- inputSearch
  ((game, record), status) <- simulate search
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