{-# LANGUAGE FlexibleInstances #-}


module Data.Tsuro.Read where

import Control.Monad
import Data.Ix
import Data.Char
import Data.List
import Data.Tsuro
import Ziphil.Util.Core
import Ziphil.Util.List
import Ziphil.Util.Monad


class ReadRec a where
  readRec :: String -> Maybe a

instance ReadRec Rotation where
  readRec "T" = Just None
  readRec "R" = Just Clock
  readRec "B" = Just Inverse
  readRec "L" = Just Anticlock
  readRec _ = Nothing

instance ReadRec TilePos where
  readRec string = guard (length string == 2) >> outA (parseX $ string !! 1, parseY $ string !! 0)
    where
      parseX char = elemIndex char "ABCDEF"
      parseY char = guard (inRange (1, 6) $ digitToInt char) >> Just (digitToInt char - 1)

instance ReadRec GameMove where
  readRec string = guard (length string == 3) >> outA (readRec $ string !!& [0, 1], readRec $ string !!& [2])