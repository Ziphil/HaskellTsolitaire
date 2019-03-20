--


module Main where

import Data.Either
import Data.Tsuro
import Data.Tsuro.Interface
import qualified Data.Tsuro.Search.Core as Search
import qualified Data.Tsuro.Search.Montecarlo as Montecarlo
import Data.Tsuro.Show


main :: IO ()
main = simulate

profile :: IO ()
profile = move >> return ()
  where
    move = Montecarlo.search =<< fromRight undefined . gameStateOf <$> initialGame

simulate :: IO ()
simulate = putStrLn . makeString =<< simulate' =<< initialGame
  where
    makeString ((_, record), status) = show status ++ ": " ++ showRec record
    simulate' = Search.simulate Montecarlo.search