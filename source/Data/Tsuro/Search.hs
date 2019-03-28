{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}


module Data.Tsuro.Search
  ( Search (..)
  , SomeSearch (..)
  , parseSearch
  , SimulateStatus (..)
  , Record
  , SearchResult
  , Hook
  , simulate
  , simulateWithHook
  , measureRate
  )
where

import Control.Arrow
import Control.Monad.Random
import Data.Either
import Data.Tsuro.Core
import Data.Tsuro.Search.Class
import qualified Data.Tsuro.Search.Montecarlo as Montecarlo
import Ziphil.Util.Core


data SomeSearch = forall s. Search IO s => SomeSearch s

parseSearch :: String -> Maybe SomeSearch
parseSearch string =
  case string of
    "m" -> Just $ SomeSearch Montecarlo.defaultConfig
    "mn" -> Just $ SomeSearch Montecarlo.defaultConfigWithoutExtra
    "mf" -> Just $ SomeSearch Montecarlo.fastConfig
    "mfn" -> Just $ SomeSearch Montecarlo.fastConfigWithoutExtra
    _ -> Nothing

data SimulateStatus = Success | Failure
  deriving (Eq, Show)

type Record = [TileMove]
type SearchResult = (Game, Record)

type Hook m = Double -> m ()

-- 与えられた探索アルゴリズムを用いて、クリアするか詰むかするまでゲームをプレイします。
-- クリアしたか詰んだかの情報に加え、プレイ後のゲーム状況および棋譜を返します。
-- なお、7 手以下で詰んだ場合は、確定詰みのタイル順を引いてしまったと見なし、もう一度プレイをやり直します。
simulate :: (MonadRandom m, Search m s) => s -> m (SearchResult, SimulateStatus)
simulate = simulateWithHook $ return . const ()

simulateWithHook :: (MonadRandom m, Search m s) => Hook m -> s -> m (SearchResult, SimulateStatus)
simulateWithHook hook search = bool (simulateWithHook hook search) result =<< check <$> result
  where
    result = simulateRecursion hook search . (, []) =<< initialGame
    check ((_, record), status) = status == Failure && length record <= 7

simulateRecursion :: (Monad m, Search m s) => Hook m -> s -> SearchResult -> m (SearchResult, SimulateStatus)
simulateRecursion hook search result@(game, record) = 
  case (isCleared game, isOver game) of
    (True, _) -> return (result, Success)
    (False, True) -> return (result, Failure)
    (False, False) -> (hook =<< makeProgress <$> nextResult) >> (simulateRecursion hook search =<< nextResult)
  where
    nextResult = (fst &&& makeRecord) <$> simulateOnce search game
    makeProgress (_, record) = fromIntegral (length record) / fromIntegral tileSize
    makeRecord (_, move) = record ++ [move]

simulateOnce :: (Monad m, Search m s) => s -> Game -> m (Game, TileMove)
simulateOnce search game = (makeGame &&& makeTileMove) <$> runSearch search state
  where
    makeTileMove move = tileMoveOf (hand state) move
    makeGame move = fromRight (error "weird search") $ applyMove move game
    state = fromRight undefined $ gameStateOf game

-- 与えられた探索アルゴリズムを用いて複数回ゲームをプレイし、成功率を計算します。
measureRate :: (MonadRandom m, Search m s) => Int -> s -> m Double
measureRate size search = calcRate . length . filter ((== Success) . snd) <$> results
  where
    results = replicateM size $ simulate search
    calcRate successSize = fromIntegral successSize / fromIntegral size