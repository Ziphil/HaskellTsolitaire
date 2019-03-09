--


module Main where

import Control.Exception
import Control.Monad
import Data.List
import Test.Hspec
import Tsuro.Base
import Prelude


infixl 6 <@
(<@) :: Board -> TileMove -> TsuroMaybe Board
(<@) = flip putTileAndUpdate

infixl 6 <<@
(<<@) :: TsuroMaybe Board -> TileMove -> TsuroMaybe Board
(<<@) = flip $ (=<<) . putTileAndUpdate

infixl 6 <@@
(<@@) :: Game -> GameMove -> TsuroMaybe Game
(<@@) = flip applyMove

infixl 6 <<@@
(<<@@) :: TsuroMaybe Game -> GameMove -> TsuroMaybe Game
(<<@@) = flip $ (=<<) . applyMove

singleMoveTest :: SpecWith (Arg Expectation)
singleMoveTest = it "returns the resulted board after a single move" $ do
  stone `shouldBe` Right ((2, 0), LeftTop)
    where
      board = initialBoard <@ ((1, 0), Tile 25 Inverse)
      stone = (!! 0) <$> stones <$> board

multipleMoveTest :: SpecWith (Arg Expectation)
multipleMoveTest = it "returns the resulted board after multiple moves" $ do
  stone `shouldBe` Right ((2, 2), LeftBottom)
    where
      board = initialBoard <@ ((1, 0), Tile 31 None) <<@ ((1, 1), Tile 30 None) <<@ ((1, 2), Tile 21 Clock)
      stone = (!! 0) <$> stones <$> board

detachedTilePosMoveTest :: SpecWith (Arg Expectation)
detachedTilePosMoveTest = it "fails when attempting to put a tile at a position adjacent to no stones" $ do
  board `shouldBe` Left DetachedTilePos
    where
      board = initialBoard <@ ((2, 2), Tile 17 None)

outOfBoardMoveTest :: SpecWith (Arg Expectation)
outOfBoardMoveTest = it "fails when attempting to put a tile which leads a stone out of the board" $ do
  board `shouldBe` Left OutOfBoard
    where
      board = initialBoard <@ ((5, 1), Tile 11 None) <<@ ((5, 2), Tile 8 Inverse)

tileAlreadyPutMoveTest :: SpecWith (Arg Expectation)
tileAlreadyPutMoveTest = it "fails when attempting to put a tile which leads a stone out of the board" $ do
  board `shouldBe` Left TileAlreadyPut
    where
      board = initialBoard <@ ((1, 0), Tile 25 None) <<@ ((1, 0), Tile 4 Anticlock)

overTest :: SpecWith (Arg Expectation)
overTest = it "reports that the game is over when the next tile cannot be put" $ do
  isGameOver `shouldBe` Right True
    where
      firstGame = createGame $ take tileSize $ nub $ [33, 32, 0] ++ [0 ..]
      game = firstGame <@@ ((4, 0), None) <<@@ ((0, 1), Clock)
      isGameOver = isOver <$> game

test :: SpecWith ()
test = describe "Tsuro.Base" $ do
  singleMoveTest
  multipleMoveTest
  detachedTilePosMoveTest
  outOfBoardMoveTest
  tileAlreadyPutMoveTest
  overTest

main :: IO ()
main = hspec test