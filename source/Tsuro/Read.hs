{-# LANGUAGE FlexibleInstances #-}


module Tsuro.Read where

import Control.Monad
import Data.Ix
import Data.Char
import Data.List
import Tsuro.Base
import ZiphilUtil


class ReadRec a where
  readRec :: String -> Maybe a

instance ReadRec Rotation where
  readRec "T" = Just None
  readRec "R" = Just Clock
  readRec "B" = Just Inverse
  readRec "L" = Just Anticlock
  readRec _ = Nothing

instance ReadRec TilePos where
  readRec string = guard (length string == 2) >> liftMaybe (parseX $ string !! 1, parseY $ string !! 0)
    where
      parseX char = elemIndex char "ABCDEF"
      parseY char = guard (inRange (1, 6) $ digitToInt char) >> Just (digitToInt char - 1)

instance ReadRec GameMove where
  readRec string = guard (length string == 3) >> liftMaybe (readRec [string !! 0, string !! 1], readRec [string !! 2])