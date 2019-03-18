--


module Data.Tsuro.Search where

import Control.Applicative
import Data.Either
import Data.Function
import Data.List
import Data.Ord
import Data.Tsuro
import System.Random


maximumBy' :: (a -> a -> Ordering) -> [a] -> (Int, a)
maximumBy' comp = maximumBy (on comp snd) . zip [0 ..]

infixl 9 //^
(//^) :: [a] -> (Int, a) -> [a]
[] //^ _ = []
(x : xs) //^ (i, next) =
  if i == 0
    then next : xs
    else x : xs //^ (i - 1, next)

type Label = Either GameState Board

data SearchTree = Node {label :: Label, num :: Int, accum :: Double, children :: [SearchTree]}

-- 葉ノードから子ノードを展開せずにプレイアウトする回数の閾値を返します。
thresholdNum :: Int
thresholdNum = 500

ratio :: SearchTree -> Double
ratio (Node _ num accum _) = accum / fromIntegral num

score :: SearchTree -> SearchTree -> Double
score node child = ratio child

-- 指定されたノードからモンテカルロ木探索を 1 ステップ実行し、実行後のノードとプレイアウトの報酬値を返します。
montecarlo :: RandomGen g => g -> SearchTree -> (SearchTree, Double)
montecarlo gen node@(Node label num accum children) =
  if null children
    then if num < thresholdNum
      then montecarloPlayout gen node
      else montecarloExpand gen node
    else montecarloRecursion gen node

montecarloPlayout :: RandomGen g => g -> SearchTree -> (SearchTree, Double)
montecarloPlayout gen (Node label num accum _) = (nextTree, reward)
  where
    nextTree = Node label (num + 1) (accum + reward) []
    reward = playout gen label

montecarloExpand :: RandomGen g => g -> SearchTree -> (SearchTree, Double) 
montecarloExpand gen (Node label num accum _) = montecarloRecursion gen nextTree
  where
    nextTree = Node label num accum (makeChildren label)

makeChildren :: Label -> [SearchTree]
makeChildren = either makeChildrenGS makeChildrenB

makeChildrenGS :: GameState -> [SearchTree]
makeChildrenGS state@(GameState board hand) = map (makeNode . makeBoard) $ possibleMoves' state
  where
    makeNode board = Node (Right board) 0 0 []
    makeBoard move = fromRight undefined $ applyMove' move state

makeChildrenB :: Board -> [SearchTree]
makeChildrenB board = map makeNode $ remainingTiles board
  where
    makeNode tile = Node (Left (GameState board tile)) 0 0 []

montecarloRecursion :: RandomGen g => g -> SearchTree -> (SearchTree, Double)
montecarloRecursion gen node@(Node label num accum children) = (nextTree, reward)
  where
    nextTree = Node label (num + 1) (accum + reward) (children //^ (index, tree))
    (tree, reward) = montecarlo gen child
    (index, child) = maximumBy' (comparing $ score node) children

-- 指定された状態からプレイアウトを実行し、その結果となる報酬値を返します。
-- 報酬値は 0 以上 1 以下の数で、1 に近いほどプレイヤーにとって有利であったことを示します。
playout :: RandomGen g => g -> Label -> Double
playout = liftA2 either playoutGS playoutB

playoutGS :: RandomGen g => g -> GameState -> Double
playoutGS gen state = undefined

playoutB :: RandomGen g => g -> Board -> Double
playoutB gen board = undefined

search :: RandomGen g => g -> GameState -> GameMove
search state = undefined