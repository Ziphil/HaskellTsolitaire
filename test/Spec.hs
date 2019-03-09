--


module Main where

import Control.Exception
import Control.Monad
import Data.List
import Test.Hspec
import Tsuro.Base
import Prelude


singleMoveTest :: SpecWith (Arg Expectation)
singleMoveTest = it "returns the resulted board after a single move" $ do
  stone `shouldBe` ((2, 0), LeftTop)
    where
      board = initialBoard <!~ ((1, 0), Tile 25 Inverse)
      stone = stones board !! 0

multipleMoveTest :: SpecWith (Arg Expectation)
multipleMoveTest = it "returns the resulted board after multiple moves" $ do
  firstStone `shouldBe` ((1, 1), TopLeft)
  secondStone `shouldBe` ((1, 2), TopRight)
  thirdStone `shouldBe` ((2, 2), LeftBottom)
    where
      firstBoard = initialBoard <!~ ((1, 0), Tile 31 None)
      secondBoard = firstBoard <!~ ((1, 1), Tile 30 Clock)
      thirdBoard = secondBoard <!~ ((1, 2), Tile 21 Clock)
      firstStone = stones firstBoard !! 0
      secondStone = stones secondBoard !! 0
      thirdStone = stones thirdBoard !! 0

detachedTilePosMoveTest :: SpecWith (Arg Expectation)
detachedTilePosMoveTest = it "fails when attempting to put a tile at a position adjacent to no stones" $ do
  board `shouldBe` Left DetachedTilePos
    where
      board = initialBoard <<~ ((2, 2), Tile 17 None)

outOfBoardMoveTest :: SpecWith (Arg Expectation)
outOfBoardMoveTest = it "fails when attempting to put a tile which leads a stone out of the board" $ do
  secondBoard `shouldBe` Left OutOfBoard
    where
      firstBoard = initialBoard <!~ ((5, 1), Tile 11 None)
      secondBoard = firstBoard <<~ ((5, 2), Tile 8 Inverse)

tileAlreadyPutMoveTest :: SpecWith (Arg Expectation)
tileAlreadyPutMoveTest = it "fails when attempting to put a tile which leads a stone out of the board" $ do
  secondBoard `shouldBe` Left TileAlreadyPut
    where
      firstBoard = initialBoard <!~ ((1, 0), Tile 25 None)
      secondBoard = firstBoard <<~ ((1, 0), Tile 4 Anticlock)

overTest :: SpecWith (Arg Expectation)
overTest = it "" $ do
  isOver' `shouldBe` Right True
    where
      firstGame = createGame $ take tileSize $ nub $ [33, 0] ++ [0 ..]
      game = move (4, 0) None firstGame
      isOver' = isOver <$> game

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