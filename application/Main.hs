--


module Main where

import Data.Either
import Data.Tsuro
import Data.Tsuro.Interface
import Data.Tsuro.Search.Montecarlo
import Data.Tsuro.Show


main :: IO ()
main = start

profile :: IO ()
profile = move >> return ()
  where
    move = search =<< fromRight undefined . gameStateOf <$> initialGame