--


module Main where

import Data.Either
import Data.Tsuro
import qualified Data.Tsuro.Interface.Game as Game
import qualified Data.Tsuro.Search.Core as Search
import qualified Data.Tsuro.Search.Montecarlo as Montecarlo
import Data.Tsuro.Show


main :: IO ()
main = simulate

profile :: IO ()
profile = move >> return ()
  where
    move = Search.runSearch Montecarlo.defaultConfig =<< fromRight undefined . gameStateOf <$> initialGame

simulate :: IO ()
simulate = putStrLn . makeString =<< result
  where
    makeString ((game, record), status) = show status ++ ": " ++ showRich record ++ " -> " ++ either (const "") (show . number) (nextHand game) ++ "\n" ++ showRich game
    result = Search.simulate Montecarlo.defaultConfig