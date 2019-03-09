{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module Tsuro.Show where

import Data.Array
import Data.List
import Tsuro.Base
import Prelude


class ShowRec a where
  showRec :: a -> String

instance ShowRec Rotation where
  showRec None = "T"
  showRec Clock = "R"
  showRec Inverse = "B"
  showRec Anticlock = "L"

instance ShowRec Edge where
  showRec TopLeft = "tl"
  showRec TopRight = "tr"
  showRec RightTop = "rt"
  showRec RightBottom = "rb"
  showRec BottomRight = "br"
  showRec BottomLeft = "bl"
  showRec LeftBottom = "lb"
  showRec LeftTop = "lt"

instance ShowRec Tile where
  showRec (Tile number rotation) = show number ++ showRec rotation

instance ShowRec TilePos where
  showRec (x, y) = show (y + 1) ++ ["ABCDEF" !! x]

pad :: Int -> String -> String
pad size string = replicate (size - length string) ' ' ++ string

instance ShowRec Tiles where
  showRec (Tiles tiles) = wholeString
    where 
      wholeString = intercalate "\n" $ map rowString boardList
      rowString y = "[ " ++ unwords (rowList y) ++ " ]"
      rowList y = map (pad 3 . showRec' . (tiles !) . (, y)) boardList
      showRec' = maybe ". " showRec
      boardList = [0 .. boardSize - 1]

instance ShowRec [StonePos] where
  showRec stones = intercalate ", " $ map showRec' stones
    where
      showRec' (tilePos, edge) = showRec tilePos ++ showRec edge

instance ShowRec Board where
  showRec (Board tiles stones) = showRec tiles ++ "\n" ++ showRec stones

instance ShowRec Game where
  showRec game = showRec (board game) ++ "\nNext: " ++ nextHandString
    where nextHandString = either (const "") showRec (nextHand game)