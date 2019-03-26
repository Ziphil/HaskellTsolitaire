--


module Data.Tsuro.Interface.MeasureRate
  ( start
  )
where

import Data.Tsuro.Core
import Data.Tsuro.Interface.Show
import Data.Tsuro.Interface.Util
import Data.Tsuro.Search
import Text.Printf
import Text.Read


start :: IO ()
start = do
  SomeSearch search <- inputSearch
  size <- inputSize
  rate <- measureRate size search
  flushStrLn ""
  flushStrLn $ colorMessage $ "@ Success Rate: " ++ printf "%.2f" (rate * 100) ++ "%"

inputSearch :: IO SomeSearch
inputSearch = do
  flushStr $ colorInput "<?> Algorithm -> "
  input <- getLine
  case parseSearch input of
    Just search -> return search
    Nothing -> do
      flushStrLn $ colorError "@ No such algorithm."
      inputSearch

inputSize :: IO Int
inputSize = do
  flushStr $ colorInput "<?> Size -> "
  input <- getLine
  case readMaybe input of
    Just size -> return size
    Nothing -> do
      flushStrLn $ colorError "@ Invalid input."
      inputSize