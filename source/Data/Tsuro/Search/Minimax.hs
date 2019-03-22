{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Data.Tsuro.Search.Minimax where

import Control.Arrow
import Data.Either
import Data.List
import Data.Ord
import Data.Reflection
import Data.Tsuro
import Data.Tsuro.Search.Core


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

wholeGameTree :: GameState -> GameTree
wholeGameTree = error "to be implemented"

search :: Given Config => GameState -> GameMove
search state = make $ wholeGameTree state
  where
    make = snd . fromRight undefined . label . maximumBy (comparing $ minimax 0) . children

searchWithRatio :: Given Config => GameState -> (GameMove, Double)
searchWithRatio state = make $ wholeGameTree state
  where
    make = (snd . fromRight undefined . label . fst &&& snd) . maximumBy (comparing snd) . map (id &&& minimax 0) . children

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

evaluate :: Label -> Double
evaluate label = either evaluateS (evaluateB . fst) label

evaluateS :: GameState -> Double
evaluateS state = error "to be implemented"

evaluateB :: Board -> Double
evaluateB board = error "to be implemented"