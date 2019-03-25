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
  , simulate
  , measureRate
  )
where

import Control.Arrow
import Control.Monad.Random
import Data.Either
import Data.Functor.Identity
import Data.Tsuro.Core
import Data.Tsuro.Search.Class
import qualified Data.Tsuro.Search.Montecarlo as Montecarlo
import Ziphil.Util.Core


data SomeSearch = forall s. Search IO s => SomeSearch s

parseSearch :: String -> Maybe SomeSearch
parseSearch string =
  case string of
    "m" -> Just $ SomeSearch Montecarlo.defaultConfig
    "mf" -> Just $ SomeSearch Montecarlo.fastConfig
    _ -> Nothing

data SimulateStatus = Success | Failure
  deriving (Eq, Show)

type Record = [TileMove]
type SearchResult = (Game, Record)

-- 与えられた探索アルゴリズムを用いて、クリアするか詰むかするまでゲームをプレイします。
-- クリアしたか詰んだかの情報に加え、プレイ後のゲーム状況および棋譜を返します。
simulate :: (MonadRandom m, Search m s) => s -> m (SearchResult, SimulateStatus)
simulate search = simulateRecursion search . (, []) =<< initialGame

simulateRecursion :: (Monad m, Search m s) => s -> SearchResult -> m (SearchResult, SimulateStatus)
simulateRecursion search result@(game, record) = 
  case (isCleared game, isOver game) of
    (True, _) -> return (result, Success)
    (False, True) -> return (result, Failure)
    (False, False) -> simulateRecursion search =<< (fst &&& makeRecord) <$> simulateOnce search game
  where
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