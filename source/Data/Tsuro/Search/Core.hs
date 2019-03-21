--


module Data.Tsuro.Search.Core where

import Control.Arrow
import Data.Either
import Data.Functor.Identity
import Data.Tsuro


data SimulateStatus = Success | Failure
  deriving (Eq, Show)

type Search = GameState -> GameMove
type RandomSearch m = GameState -> m GameMove

type Record = [TileMove]
type SearchResult = (Game, Record)

simulate :: Search -> Game -> (SearchResult, SimulateStatus)
simulate search game = runIdentity $ simulate' (return . search) game

simulate' :: Monad m => RandomSearch m -> Game -> m (SearchResult, SimulateStatus)
simulate' search game = simulateRecursion search (game, [])

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