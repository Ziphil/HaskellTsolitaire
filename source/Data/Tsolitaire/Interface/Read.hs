{-# LANGUAGE FlexibleInstances #-}


module Data.Tsolitaire.Interface.Read
  ( ReadRich (..)
  )
where

import Control.Monad
import Data.Char
import Data.Ix
import Data.List
import Data.Tsolitaire.Core
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

parseX :: Char -> Maybe Int
parseX char = elemIndex char "ABCDEF"

parseY :: Char -> Maybe Int
parseY char = guard (isDigit char) >> guard (inRange (0, 5) y) >> Just y
  where
    y = digitToInt char - 1

instance ReadRich TilePos where
  readRich string = guard (length string == 2) >> outA (parseX $ string !! 1, parseY $ string !! 0)

instance ReadRich GameMove where
  readRich string = guard (length string == 3) >> outA (readRich $ string !!& [0, 1], readRich $ string !!& [2])