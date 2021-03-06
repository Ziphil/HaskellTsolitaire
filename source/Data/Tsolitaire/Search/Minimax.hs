{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tsolitaire.Search.Minimax
  ( Config (..)
  , defaultConfig
  )
where

import Control.Arrow
import Data.Either
import Data.List
import Data.Ord
import Data.Reflection
import Data.Tsolitaire.Core
import Data.Tsolitaire.Search.Class


data Config = Config {maxDepth :: Int}

instance Monad m => Search m Config where
  runSearch config = return . give config search
  runSearchWithRatio config = return . give config searchWithRatio

defaultConfig :: Config
defaultConfig = error "to be implemented"

type BoardLabel = (Board, GameMove)
type Label = Either GameState BoardLabel

data GameTree = Node {label :: Label, children :: [GameTree]}
  deriving (Eq, Show)

search :: Given Config => GameState -> GameMove
search state = make $ wholeGameTree state
  where
    make = snd . fromRight undefined . label . maximumBy (comparing $ minimax 0) . children

searchWithRatio :: Given Config => GameState -> (GameMove, Double)
searchWithRatio state = make $ wholeGameTree state
  where
    make = first (snd . fromRight undefined . label) . maximumBy (comparing snd) . map (id &&& minimax 0) . children

minimax :: Given Config => Int -> GameTree -> Double
minimax depth node@(Node label children) = 
  if null children
    then minimaxLeaf depth node
    else if depth >= maxDepth given
      then evaluate label
      else minimaxRecursion depth node

minimaxLeaf :: Int -> GameTree -> Double
minimaxLeaf _ (Node label _) = either (const 0) (const 1) label

minimaxRecursion :: Given Config => Int -> GameTree -> Double
minimaxRecursion depth (Node label children) = calc $ map (minimax (depth + 1)) children
  where
    calc = either (const maximum) (const minimum) label

wholeGameTree :: GameState -> GameTree
wholeGameTree state = makeNode $ Left state

makeChildren :: Label -> [GameTree]
makeChildren = either makeChildrenS (makeChildrenB . fst)

makeNode :: Label -> GameTree
makeNode label = Node label $ makeChildren label

makeChildrenS :: GameState -> [GameTree]
makeChildrenS state@(GameState board hand) = map makeNode' $ possibleMovesAndBoards' state
  where
    makeNode' (move, board) = makeNode $ Right (board, move)

makeChildrenB :: Board -> [GameTree]
makeChildrenB board = map makeNode' $ remainingTiles board
  where
    makeNode' tile = makeNode $ Left (GameState board tile)

evaluate :: Label -> Double
evaluate = either evaluateS (evaluateB . fst)

evaluateS :: GameState -> Double
evaluateS state = error "to be implemented"

evaluateB :: Board -> Double
evaluateB board = error "to be implemented"