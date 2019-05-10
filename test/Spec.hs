--


module Main where

import Control.Exception
import Control.Monad
import Data.List
import Data.Tsolitaire
import Test.Hspec
import Ziphil.Util.Test


infixl 6 <@
(<@) :: Board -> TileMove -> WithInvalid Board
(<@) = flip putTile

infixl 6 <<@
(<<@) :: WithInvalid Board -> TileMove -> WithInvalid Board
(<<@) = flip $ (=<<) . putTile

infixl 6 <@@
(<@@) :: Game -> GameMove -> WithInvalid Game
(<@@) = flip applyMove

infixl 6 <<@@
(<<@@) :: WithInvalid Game -> GameMove -> WithInvalid Game
(<<@@) = flip $ (=<<) . applyMove

singleMoveTest :: SpecWith (Arg Expectation)
singleMoveTest = it "returns the resulted board after a single move" $ do
  stone ?== Right ((2, 0), LeftTop)
    where
      board = initialBoard <@ ((1, 0), Tile 25 Inverse)
      stone = (!! 0) . stones <$> board

multipleMoveTest :: SpecWith (Arg Expectation)
multipleMoveTest = it "returns the resulted board after multiple moves" $ do
  stone ?== Right ((2, 2), LeftBottom)
    where
      board = initialBoard <@ ((1, 0), Tile 31 None) <<@ ((1, 1), Tile 30 None) <<@ ((1, 2), Tile 21 Clock)
      stone = (!! 0) . stones <$> board

detachedTilePosMoveTest :: SpecWith (Arg Expectation)
detachedTilePosMoveTest = it "fails when attempting to put a tile at a position adjacent to no stones" $ do
  board ?== Left DetachedTilePos
    where
      board = initialBoard <@ ((2, 2), Tile 17 None)

outOfBoardMoveTest :: SpecWith (Arg Expectation)
outOfBoardMoveTest = it "fails when attempting to put a tile which leads a stone out of the board" $ do
  board ?== Left OutOfBoard
    where
      board = initialBoard <@ ((5, 1), Tile 11 None) <<@ ((5, 2), Tile 8 Inverse)

tileAlreadyPutMoveTest :: SpecWith (Arg Expectation)
tileAlreadyPutMoveTest = it "fails when attempting to put a tile where some tile is already put" $ do
  board ?== Left TileAlreadyPut
    where
      board = initialBoard <@ ((1, 0), Tile 25 None) <<@ ((1, 0), Tile 4 Anticlock)

overTest :: SpecWith (Arg Expectation)
overTest = it "reports that the game is over when the next tile cannot be put" $ do
  isOver <$> game ?== Right True
    where
      firstGame = createGame $ take tileSize $ nub $ [33, 32, 0] ++ [0 ..]
      game = firstGame <@@ ((4, 0), None) <<@@ ((0, 1), Clock)

wholePlayTest :: SpecWith (Arg Expectation)
wholePlayTest = it "" $ do
  hand ?== Right (Tile 29 None)
  possibleMoves <$> game ?== Right [((2, 1), Clock)]
  isOver <$> game ?== Right False
  isCleared <$> finalGame ?== Right True
    where
      firstGame = createGame [31, 25, 34, 30, 24, 7, 28, 33, 26, 2, 18, 5, 14, 32, 17, 21, 16, 0, 19, 6, 8, 10, 13, 23, 27, 3, 20, 9, 11, 15, 1, 12, 4, 22, 29]
      game = firstGame
        <@@ ((4, 0), None) <<@@ ((1, 5), Inverse) <<@@ ((0, 4), None) <<@@ ((1, 0), Clock) <<@@ ((4, 5), Clock) <<@@ ((0, 3), Anticlock) <<@@ ((5, 1), Inverse)
        <<@@ ((0, 1), None) <<@@ ((1, 3), Clock) <<@@ ((5, 0), None) <<@@ ((4, 1), None) <<@@ ((0, 5), Clock) <<@@ ((5, 5), Anticlock) <<@@ ((5, 4), None)
        <<@@ ((4, 2), Inverse) <<@@ ((1, 1), None) <<@@ ((4, 4), Clock) <<@@ ((0, 0), None) <<@@ ((5, 3), Clock) <<@@ ((2, 5), Inverse) <<@@ ((5, 2), Anticlock)
        <<@@ ((3, 5), Inverse) <<@@ ((1, 2), Clock) <<@@ ((0, 2), Clock) <<@@ ((2, 3), None) <<@@ ((3, 0), Anticlock) <<@@ ((3, 4), Inverse) <<@@ ((2, 0), None)
        <<@@ ((2, 4), Clock) <<@@ ((4, 3), Inverse) <<@@ ((3, 3), None) <<@@ ((1, 4), None) <<@@ ((3, 2), None) <<@@ ((3, 1), Clock)
      hand = nextHand =<< game
      finalGame = game <<@@ ((2, 1), Clock)

test :: SpecWith ()
test = describe "Tsuro.Base" $ do
  singleMoveTest
  multipleMoveTest
  detachedTilePosMoveTest
  outOfBoardMoveTest
  tileAlreadyPutMoveTest
  overTest
  wholePlayTest

main :: IO ()
main = hspec test