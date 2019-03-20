--


module Data.Tsuro.Search.Core where

import Control.Arrow
import Data.Either
import Data.Tsuro


data SimulateStatus = Success | Failure
  deriving (Eq, Show)

type Search m = GameState -> m GameMove

type Record = [TileMove]
type SearchResult = (Game, Record)

simulate :: Monad m => Search m -> Game -> m (SearchResult, SimulateStatus)
simulate search game = simulate' search (game, [])

simulate' :: Monad m => Search m -> SearchResult -> m (SearchResult, SimulateStatus)
simulate' search result@(game, record) = 
  case (isCleared game, isOver game) of
    (True, _) -> return (result, Success)
    (False, True) -> return (result, Failure)
    (False, False) -> simulate' search =<< (fst &&& makeRecord) <$> simulateOnce search game
  where
    makeRecord (_, move) = record ++ [move]

simulateOnce :: Monad m => Search m -> Game -> m (Game, TileMove)
simulateOnce search game = (makeGame &&& makeTileMove) <$> search state
  where
    makeTileMove move = tileMoveOf hand move
    makeGame move = fromRight (error "weird search") $ applyMove move game
    hand = fromRight undefined $ nextHand game
    state = fromRight undefined $ gameStateOf game