{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module Tsuro.Show where

import Data.Array
import Data.List
import System.Console.Pretty
import Tsuro.Base
import ZiphilUtil


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

colorTile :: Pretty a => a -> a
colorTile = style Reverse

colorTurn :: Pretty a => a -> a
colorTurn = style Underline . color Black

instance ShowRec Tiles where
  showRec (Tiles tiles) = wholeString
    where 
      wholeString = intercalate "\n" $ map rowString boardList
      rowString y = interpose "[ " " ]" $ unwords $ rowList y
      rowList y = map (showRec' . (tiles !) . (, y)) boardList
      showRec' = maybe " . " (colorTile . pad 3 . showRec)
      boardList = [0 .. boardSize - 1]

instance ShowRec [StonePos] where
  showRec stones = unwords $ map showRec' stones
    where
      showRec' (tilePos, edge) = showRec tilePos ++ showRec edge

instance ShowRec Board where
  showRec (Board tiles stones) = showRec tiles ++ " " ++ showRec stones

instance ShowRec GameMove where
  showRec (pos, rotation) = showRec pos ++ showRec rotation

instance ShowRec Game where
  showRec (Game board hands) = turnString ++ "\n" ++ showRec board
    where
      turnString = colorTurn $ "Turn " ++ show turn
      turn = tileSize - length hands + 1