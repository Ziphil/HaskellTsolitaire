--


module Main where

import Control.Exception
import Control.Monad
import Test.Hspec
import Tsuro.Base
import Prelude hiding (Left, Right)


singleMoveTest :: SpecWith (Arg Expectation)
singleMoveTest = it "returns the resulted board after a single move" $ do
  shouldBe stone ((2, 0), LeftTop)
    where
      board = initialBoard <!~ ((1, 0), Tile 25 Inverse)
      stone = stones board !! 0

multipleMoveTest :: SpecWith (Arg Expectation)
multipleMoveTest = it "returns the resulted board after multiple moves" $ do
  shouldBe firstStone ((1, 1), TopLeft)
  shouldBe secondStone ((1, 2), TopRight)
  shouldBe thirdStone ((2, 2), LeftBottom)
    where
      firstBoard = initialBoard <!~ ((1, 0), Tile 31 None)
      secondBoard = firstBoard <!~ ((1, 1), Tile 30 Clock)
      thirdBoard = secondBoard <!~ ((1, 2), Tile 21 Clock)
      firstStone = stones firstBoard !! 0
      secondStone = stones secondBoard !! 0
      thirdStone = stones thirdBoard !! 0

invalidMoveTest :: SpecWith (Arg Expectation)
invalidMoveTest = it "fails when an invalid move is passed" $ do
  shouldThrow (evaluate board) anyException
    where board = initialBoard <!~ ((2, 2), Tile 17 None)

test :: SpecWith ()
test = describe "Tsuro.Base" $ do
  singleMoveTest
  multipleMoveTest
  invalidMoveTest

main :: IO ()
main = hspec test