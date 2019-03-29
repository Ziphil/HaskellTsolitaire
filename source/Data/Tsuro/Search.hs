{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}


module Data.Tsuro.Search
  ( Search (..)
  , SomeSearch (..)
  , parseSearch
  , SimulationStatus (..)
  , Record
  , GameHistory
  , SimulationResult
  , Hook
  , simulate
  , simulateWithHook
  , measureRate
  , measureRateWithHook
  )
where

import Control.Arrow
import Control.Monad.Random
import Data.Bool
import Data.Either
import Data.Tsuro.Core
import Data.Tsuro.Search.Class
import qualified Data.Tsuro.Search.Montecarlo as Montecarlo


data SomeSearch = forall s. Search IO s => SomeSearch s

parseSearch :: String -> Maybe SomeSearch
parseSearch string =
  case string of
    "m" -> Just $ SomeSearch Montecarlo.defaultConfig
    "mn" -> Just $ SomeSearch Montecarlo.defaultConfigWithoutExtra
    "mf" -> Just $ SomeSearch Montecarlo.fastConfig
    "mfn" -> Just $ SomeSearch Montecarlo.fastConfigWithoutExtra
    _ -> Nothing

data SimulationStatus = Success | Failure
  deriving (Eq, Show)

type Record = [TileMove]
type GameHistory = (Game, Record)
type SimulationResult = (GameHistory, SimulationStatus)

-- 与えられたシミュレーションの結果から、確定詰みのタイル順を引いてしまった可能性があるかどうか判定します。
-- 現状では 7 手以下で詰んでいた場合に True を返します。
isDefiniteOver :: SimulationResult -> Bool
isDefiniteOver ((_, record), status) = status == Failure && length record <= 7

type Hook m = Double -> m ()

-- 与えられた探索アルゴリズムを用いて、クリアするか詰むかするまでゲームをプレイします。
-- クリアしたか詰んだかの情報に加え、プレイ後のゲーム状況および棋譜を返します。
-- なお、確定詰みのタイル順を引いてしまったと見なされた場合は、もう一度プレイをやり直します。
simulate :: (MonadRandom m, Search m s) => s -> m SimulationResult
simulate = simulateWithHook $ return . const ()

simulateWithHook :: (MonadRandom m, Search m s) => Hook m -> s -> m SimulationResult
simulateWithHook hook search = make =<< simulateRecursion hook search . (, []) =<< initialGame
  where
    make result = bool (simulateWithHook hook search) (return result) $ isDefiniteOver result 

simulateRecursion :: (Monad m, Search m s) => Hook m -> s -> GameHistory -> m SimulationResult
simulateRecursion hook search history@(game, record) = 
  case (isCleared game, isOver game) of
    (True, _) -> return (history, Success)
    (False, True) -> return (history, Failure)
    (False, False) -> makeResult =<< (fst &&& makeRecord) <$> simulateOnce search game
  where
    makeResult history = (hook $ calcProgress history) >> (simulateRecursion hook search history)
    calcProgress (_, record) = fromIntegral (length record) / fromIntegral tileSize
    makeRecord (_, move) = record ++ [move]

simulateOnce :: (Monad m, Search m s) => s -> Game -> m (Game, TileMove)
simulateOnce search game = (makeGame &&& makeTileMove) <$> runSearch search state
  where
    makeTileMove move = tileMoveOf (hand state) move
    makeGame move = fromRight (error "weird search") $ applyMove move game
    state = fromRight undefined $ gameStateOf game

-- 与えられた探索アルゴリズムを用いて複数回ゲームをプレイし、成功率を計算します。
measureRate :: (MonadRandom m, Search m s) => Int -> s -> m Double
measureRate = measureRateWithHook $ return . const ()

measureRateWithHook :: (MonadRandom m, Search m s) => Hook m -> Int -> s -> m Double
measureRateWithHook hook size search = calcRate . length . filter ((== Success) . snd) <$> results
  where
    results = forM [0 .. size - 1] simulate'
    simulate' i = simulateWithHook (hook . (/ fromIntegral size) . (+ fromIntegral i)) search
    calcRate successSize = fromIntegral successSize / fromIntegral size