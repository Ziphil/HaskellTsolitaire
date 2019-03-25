{-# LANGUAGE MultiParamTypeClasses #-}


module Data.Tsuro.Search.Class
  ( Search (..)
  )
where

import Data.Tsuro.Core


class Search m s where
  runSearch :: s -> GameState -> m GameMove
  runSearchWithRatio :: s -> GameState -> m (GameMove, Double)