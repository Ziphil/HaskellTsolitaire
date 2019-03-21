{-# LANGUAGE TupleSections #-}


module Data.Tsuro.Search.Core where

import Control.Arrow
import Control.Monad.Random
import Data.Either
import Data.Functor.Identity
import Data.Tsuro


data SimulateStatus = Success | Failure
  deriving (Eq, Show)

type Search = GameState -> GameMove
type RandomSearch m = GameState -> m GameMove

type Record = [TileMove]
type SearchResult = (Game, Record)

-- 与えられた探索アルゴリズムを用いて、クリアするか詰むかするまでゲームをプレイします。
-- クリアしたか詰んだかの情報に加え、プレイ後のゲーム状況および棋譜を返します。
simulate :: MonadRandom m => Search -> m (SearchResult, SimulateStatus)
simulate search = simulate' (return . search)

simulate' :: MonadRandom m => RandomSearch m -> m (SearchResult, SimulateStatus)
simulate' search = simulateRecursion search . (, []) =<< initialGame

simulateRecursion :: Monad m => RandomSearch m -> SearchResult -> m (SearchResult, SimulateStatus)
simulateRecursion search result@(game, record) = 
  case (isCleared game, isOver game) of
    (True, _) -> return (result, Success)
    (False, True) -> return (result, Failure)
    (False, False) -> simulateRecursion search =<< (fst &&& makeRecord) <$> simulateOnce search game
  where
    makeRecord (_, move) = record ++ [move]

simulateOnce :: Monad m => RandomSearch m -> Game -> m (Game, TileMove)
simulateOnce search game = (makeGame &&& makeTileMove) <$> search state
  where
    makeTileMove move = tileMoveOf (hand state) move
    makeGame move = fromRight (error "weird search") $ applyMove move game
    state = fromRight undefined $ gameStateOf game

-- 与えられた探索アルゴリズムを用いて複数回ゲームをプレイし、成功率を計算します。
measureRate :: MonadRandom m => Int -> Search -> m Double
measureRate size search = measureRate' size (return . search)

measureRate' :: MonadRandom m => Int -> RandomSearch m -> m Double
measureRate' size search = calcRate . length . filter ((== Success) . snd) <$> results
  where
    results = replicateM size $ simulate' search
    calcRate successSize = (fromIntegral successSize) / (fromIntegral size)