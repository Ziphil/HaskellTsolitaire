{-# LANGUAGE FlexibleInstances #-}


module Data.Tsuro.Read
  ( ReadRich (..)
  )
where

import Control.Monad
import Data.Ix
import Data.Char
import Data.List
import Data.Tsuro
import Ziphil.Util.Core
import Ziphil.Util.List
import Ziphil.Util.Monad


class ReadRich a where
  readRich :: String -> Maybe a

instance ReadRich Rotation where
  readRich "T" = Just None
  readRich "R" = Just Clock
  readRich "B" = Just Inverse
  readRich "L" = Just Anticlock
  readRich _ = Nothing

instance ReadRich TilePos where
  readRich string = guard (length string == 2) >> outA (parseX $ string !! 1, parseY $ string !! 0)
    where
      parseX char = elemIndex char "ABCDEF"
      parseY char = guard (isDigit char) >> guard (inRange (1, 6) $ digitToInt char) >> Just (digitToInt char - 1)

instance ReadRich GameMove where
  readRich string = guard (length string == 3) >> outA (readRich $ string !!& [0, 1], readRich $ string !!& [2])