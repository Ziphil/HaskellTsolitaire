{-# LANGUAGE MultiParamTypeClasses #-}


module Data.Tsolitaire.Search.Class
  ( Search (..)
  )
where

import Data.Tsolitaire.Core


class Search m s where
  runSearch :: s -> GameState -> m GameMove
  runSearchWithRatio :: s -> GameState -> m (GameMove, Double)