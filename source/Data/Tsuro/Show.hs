{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module Data.Tsuro.Show where

import Data.Array.IArray
import Data.List
import Data.Tsuro
import System.Console.Pretty
import Ziphil.Util.List


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
  showRec tile = show number ++ showRec rotation
    where
      Tile number rotation = normalize tile

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
      showRec' = maybe " . " (colorTile . padl 3 ' ' . showRec)
      boardList = [0 .. boardSize - 1]

instance ShowRec [StonePos] where
  showRec stones = unwords $ map showRec' stones
    where
      showRec' (tilePos, edge) = showRec tilePos ++ showRec edge

instance ShowRec Board where
  showRec (Board tiles _ stones) = showRec tiles ++ " " ++ showRec stones

instance ShowRec TileMove where
  showRec (pos, tile) = showRec pos ++ showRec tile

instance ShowRec [TileMove] where
  showRec moves = unwords $ map showRec moves

instance ShowRec GameMove where
  showRec (pos, rotation) = showRec pos ++ showRec rotation

instance ShowRec Game where
  showRec (Game board hands) = turnString ++ "\n" ++ showRec board
    where
      turnString = colorTurn $ "Turn " ++ show turn
      turn = tileSize - length hands + 1