--


module Main where

import Data.Either
import Data.Tsuro
import qualified Data.Tsuro.Interface as Interface
import qualified Data.Tsuro.Search.Montecarlo as Montecarlo


main :: IO ()
main = Interface.start

profile :: IO ()
profile = move >> return ()
  where
    move = runSearch Montecarlo.defaultConfig =<< fromRight undefined . gameStateOf <$> initialGame