--


module Data.Tsuro.Search where

import Control.Applicative
import Control.Arrow
import Control.Monad.Random
import Data.Either
import Data.Function
import Data.List
import Data.Ord
import Data.Tsuro
import System.Random
import Ziphil.Util.Core


maximumBy' :: (a -> a -> Ordering) -> [a] -> (Int, a)
maximumBy' comp = maximumBy (on comp snd) . zip [0 ..]

infixl 9 //^
(//^) :: [a] -> (Int, a) -> [a]
[] //^ _ = []
(x : xs) //^ (i, next) =
  if i == 0
    then next : xs
    else x : xs //^ (i - 1, next)

type BoardInfo = (Board, GameMove)
type Label = Either GameState BoardInfo

data SearchTree = Node {label :: Label, num :: Int, accum :: Double, children :: [SearchTree]}

-- 葉ノードから子ノードを展開せずにプレイアウトする回数の閾値を返します。
thresholdNum :: Int
thresholdNum = 500

searchSize :: Int
searchSize = 100000

expParam :: Double
expParam = 1.41

-- そのノードからの探索における報酬値の平均を返します。
ratio :: SearchTree -> Double
ratio (Node _ num accum _) = 
  if num == 0
    then 0
    else accum / fromIntegral num

-- 探索回数が少ないノードもある程度探索されるようにするための補正項を返します。
-- ここでは、UCT (upper confidence bound applied to tree) に基づく値を返します。
correction :: SearchTree -> SearchTree -> Double
correction parent child = 
  if num child == 0
    then 1 / 0
    else sqrt ((log $ fromIntegral $ num parent) / (fromIntegral $ num child)) * expParam

score :: SearchTree -> SearchTree -> Double
score parent child = ratio child + correction parent child

search :: MonadRandom m => GameState -> m GameMove
search state = snd . fromRight undefined . label . maximumBy (comparing num) . children <$> result
  where
    result = iterationList !! searchSize
    iterationList = iterate ((fst <$>) . (montecarlo =<<)) (return $ initialSearchTree state)

initialSearchTree :: GameState -> SearchTree
initialSearchTree state = Node (Left state) 0 0 (makeChildrenS state)

-- 指定されたノードからモンテカルロ木探索を 1 ステップ実行し、実行後のノードとプレイアウトの報酬値を返します。
montecarlo :: MonadRandom m => SearchTree -> m (SearchTree, Double)
montecarlo node@(Node label num accum children) =
  if null children
    then if num < thresholdNum
      then montecarloPlayout node
      else montecarloExpand node
    else montecarloRecursion node

montecarloPlayout :: MonadRandom m => SearchTree -> m (SearchTree, Double)
montecarloPlayout (Node label num accum _) = (makeNode &&& id) <$> playout label
  where
    makeNode reward = Node label (num + 1) (accum + reward) []

montecarloExpand :: MonadRandom m => SearchTree -> m (SearchTree, Double) 
montecarloExpand node@(Node label num accum _) = 
  if null children
    then montecarloLeaf node
    else montecarloRecursion nextNode
  where
    nextNode = Node label num accum children
    children = makeChildren label

montecarloRecursion :: MonadRandom m => SearchTree -> m (SearchTree, Double)
montecarloRecursion node@(Node label num accum children) = (makeNode &&& snd) <$> montecarlo child
  where
    makeNode (tree, reward) = Node label (num + 1) (accum + reward) (children //^ (index, tree))
    (index, child) = maximumBy' (comparing $ score node) children

montecarloLeaf :: MonadRandom m => SearchTree -> m (SearchTree, Double)
montecarloLeaf node@(Node label num accum _) = return (nextNode, reward)
  where
    nextNode = Node label (num + 1) (accum + reward) []
    reward = either (const 0) (const 1) label

makeChildren :: Label -> [SearchTree]
makeChildren = either makeChildrenS makeChildrenB

makeChildrenS :: GameState -> [SearchTree]
makeChildrenS state@(GameState board hand) = map makeNode $ possibleMoves' state
  where
    makeNode move = Node (Right (makeBoard move, move)) 0 0 []
    makeBoard move = fromRight undefined $ applyMove' move state

makeChildrenB :: BoardInfo -> [SearchTree]
makeChildrenB (board, _) = map makeNode $ remainingTiles board
  where
    makeNode tile = Node (Left (GameState board tile)) 0 0 []

-- 指定された状態からプレイアウトを実行し、その結果となる報酬値を返します。
-- 報酬値は 0 以上 1 以下の数で、1 に近いほどプレイヤーにとって有利であったことを示します。
playout :: MonadRandom m => Label -> m Double
playout = either playoutS (playoutB . fst)

pick :: MonadRandom m => [a] -> m a
pick list = (list !!) <$> getRandomR (0, length list - 1)

playoutS' :: MonadRandom m => GameState -> m Int
playoutS' state = (+ 1) <$> (playoutB' =<< makeNextBoard <$> move)
  where
    makeNextBoard move = fromRight undefined $ applyMove' move state
    move = pick $ possibleMoves' state

playoutB' :: MonadRandom m => Board -> m Int
playoutB' board = playoutS' =<< makeBoard <$> state
  where
    makeBoard tile = GameState board tile
    state = pick $ remainingTiles board

playoutS :: MonadRandom m => GameState -> m Double
playoutS state@(GameState board _) = 
  if null $ possibleMoves' state
    then return 0
    else ((/ max) . fromIntegral) <$> playoutS' state
  where
    max = fromIntegral $ length $ remainingTiles board

playoutB :: MonadRandom m => Board -> m Double
playoutB board =
  if null $ remainingTiles board
    then return 1
    else ((/ max) . fromIntegral) <$> playoutB' board
  where
    max = fromIntegral $ length $ remainingTiles board