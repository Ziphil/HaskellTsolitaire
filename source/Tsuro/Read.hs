{-# LANGUAGE FlexibleInstances #-}


module Tsuro.Read where

import Control.Monad
import Data.Ix
import Data.List
import Text.Regex
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

(=~) :: String -> String -> Maybe [String]
string =~ pattern = 
  case matchRegexAll (mkRegex pattern) string of
    Nothing -> Nothing
    Just (_, _, _, list) -> Just list

instance ReadRec TilePos where
  readRec string =
    case string =~ "^([0-9])([A-Za-z])$" of
      Nothing -> Nothing
      Just list -> liftMaybe (parseX $ list !! 1, parseY $ list !! 0)
        where
          parseX string = elemIndex (head string) "ABCDEF"
          parseY string = guard (inRange (1, 6) $ read string) >> Just (read string - 1)

instance ReadRec GameMove where
  readRec string = 
    case string =~ "^([0-9][A-Za-z])([A-Za-z])$" of
      Nothing -> Nothing
      Just list -> liftMaybe (readRec $ list !! 0, readRec $ list !! 1)