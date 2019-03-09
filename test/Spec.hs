--


module Main where

import Control.Exception
import Control.Monad
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

invalidMoveTest :: SpecWith (Arg Expectation)
invalidMoveTest = it "fails when an invalid move is passed" $ do
  evaluate board `shouldThrow` anyException
    where board = initialBoard <<~ ((2, 2), Tile 17 None)

test :: SpecWith ()
test = describe "Tsuro.Base" $ do
  singleMoveTest
  multipleMoveTest
  invalidMoveTest

main :: IO ()
main = hspec test