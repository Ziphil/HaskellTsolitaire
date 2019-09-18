--

module Main where

import Data.Either
import Data.Tsolitaire
import qualified Data.Tsolitaire.Interface as Interface
import qualified Data.Tsolitaire.Search.Montecarlo as Montecarlo


main :: IO ()
main = Interface.start

profile :: IO ()
profile = move >> return ()
  where
    move = runSearch Montecarlo.defaultConfig =<< fromRight undefined . gameStateOf <$> initialGame