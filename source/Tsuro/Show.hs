{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module Tsuro.Show where

import Data.Array
import Data.List
import Tsuro.Base
import Prelude hiding (Left, Right)


class ShowRec a where
  showRec :: a -> String

instance ShowRec Rotation where
  showRec Top = "T"
  showRec Right = "R"
  showRec Bottom = "B"
  showRec Left = "L"

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
      showRec' ((x, y), edge) = show (y + 1) ++ ["ABCDEF" !! x] ++ showRec edge

instance ShowRec Board where
  showRec (Board tiles stones) = showRec tiles ++ "\n" ++ showRec stones