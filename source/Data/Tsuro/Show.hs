{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module Data.Tsuro.Show where

import Data.Array.IArray
import Data.List
import Data.Tsuro
import System.Console.Pretty
import Ziphil.Util.List


class ShowRich a where
  showRich :: a -> String

instance ShowRich Rotation where
  showRich None = "T"
  showRich Clock = "R"
  showRich Inverse = "B"
  showRich Anticlock = "L"

instance ShowRich Edge where
  showRich TopLeft = "tl"
  showRich TopRight = "tr"
  showRich RightTop = "rt"
  showRich RightBottom = "rb"
  showRich BottomRight = "br"
  showRich BottomLeft = "bl"
  showRich LeftBottom = "lb"
  showRich LeftTop = "lt"

instance ShowRich Tile where
  showRich tile = show number ++ showRich rotation
    where
      Tile number rotation = normalize tile

instance ShowRich TilePos where
  showRich (x, y) = show (y + 1) ++ ["ABCDEF" !! x]

colorTile :: Pretty a => a -> a
colorTile = style Reverse

colorTurn :: Pretty a => a -> a
colorTurn = style Underline . color Black

instance ShowRich Tiles where
  showRich (Tiles tiles) = wholeString
    where 
      wholeString = intercalate "\n" $ map rowString boardList
      rowString y = interpose "[ " " ]" $ unwords $ rowList y
      rowList y = map (showRich' . (tiles !) . (, y)) boardList
      showRich' = maybe " . " (colorTile . padl 3 ' ' . showRich)
      boardList = [0 .. boardSize - 1]

instance ShowRich [StonePos] where
  showRich stones = unwords $ map showRich' stones
    where
      showRich' (tilePos, edge) = showRich tilePos ++ showRich edge

instance ShowRich Board where
  showRich (Board tiles _ _ stones) = showRich tiles ++ " " ++ showRich stones

instance ShowRich TileMove where
  showRich (pos, tile) = showRich pos ++ showRich tile

instance ShowRich [TileMove] where
  showRich moves = unwords $ map showRich moves

instance ShowRich GameMove where
  showRich (pos, rotation) = showRich pos ++ showRich rotation

instance ShowRich Game where
  showRich (Game board hands) = turnString ++ "\n" ++ showRich board
    where
      turnString = colorTurn $ "Turn " ++ show turn
      turn = tileSize - length hands + 1