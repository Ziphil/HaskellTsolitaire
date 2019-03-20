--


module Data.Tsuro.Search.Montecarlo where

import Control.Applicative
import Control.Arrow
import Control.Monad.Random
import Data.Either
import Data.List
import Data.Ord
import Data.Tsuro
import System.Random
import Ziphil.Util.List
import Ziphil.Util.Random


type BoardLabel = (Board, GameMove)
type Label = Either GameState BoardLabel

data SearchTree = Node {label :: Label, num :: Int, accum :: Double, children :: [SearchTree]}
  deriving (Eq, Show)

-- 葉ノードから子ノードを展開せずにプレイアウトする回数の閾値を返します。
thresholdNum :: Int
thresholdNum = 5

-- モンテカルロ木探索を実行するステップ数を返します。
iterateSize :: Int
iterateSize = 10000

expParam :: Double
expParam = 0.7

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

initialSearchTree :: GameState -> SearchTree
initialSearchTree state = Node (Left state) 0 0 (makeChildrenS state)

-- モンテカルロ木探索を規定回数だけ実行して、最適な手を返します。
search :: MonadRandom m => GameState -> m GameMove
search state = make <$> iterateMontecarlo (initialSearchTree state)
  where
    make = snd . fromRight undefined . label . maximumBy (comparing ratio) . children

searchWithRatio :: MonadRandom m => GameState -> m (GameMove, Double)
searchWithRatio state = make <$> iterateMontecarlo (initialSearchTree state)
  where
    make = (snd . fromRight undefined . label &&& ratio) . maximumBy (comparing ratio) . children

iterateMontecarlo :: MonadRandom m => SearchTree -> m SearchTree
iterateMontecarlo node = iterationList !! iterateSize
  where
    iterationList = iterate (montecarlo' =<<) $ return node

montecarlo' :: MonadRandom m => SearchTree -> m SearchTree
montecarlo' = (fst <$>) . montecarlo

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
    makeNode (tree, reward) = Node label (num + 1) (accum + reward) (update index tree children)
    (index, child) = maximumBy' (comparing $ score node) children

montecarloLeaf :: MonadRandom m => SearchTree -> m (SearchTree, Double)
montecarloLeaf node@(Node label num accum _) = return (nextNode, reward)
  where
    nextNode = Node label (num + 1) (accum + reward) []
    reward = either (const 0) (const 1) label

makeChildren :: Label -> [SearchTree]
makeChildren = either makeChildrenS makeChildrenB

makeChildrenS :: GameState -> [SearchTree]
makeChildrenS state@(GameState board hand) = map makeNode $ possibleMovesAndBoards' state
  where
    makeNode (move, board) = Node (Right (board, move)) 0 0 []

makeChildrenB :: BoardLabel -> [SearchTree]
makeChildrenB (board, _) = map makeNode $ remainingTiles board
  where
    makeNode tile = Node (Left (GameState board tile)) 0 0 []

-- 指定された状態からプレイアウトを実行し、その結果となる報酬値を返します。
-- 報酬値は 0 以上 1 以下の数で、1 に近いほどプレイヤーにとって有利であったことを示します。
playout :: MonadRandom m => Label -> m Double
playout = either playoutS (playoutB . fst)

playoutS' :: MonadRandom m => GameState -> m Int
playoutS' state = 
  if null movesAndBoards
    then return 0
    else (+ 1) <$> (playoutB' =<< board)
  where
    board = snd <$> pick movesAndBoards
    movesAndBoards = possibleMovesAndBoards' state

playoutB' :: MonadRandom m => Board -> m Int
playoutB' board = 
  if null tiles
    then return 0
    else playoutS' =<< state
  where
    state = GameState board <$> pick tiles
    tiles = remainingTiles board

calcReward :: Int -> Int -> Double
calcReward maxSize size =
  if size == maxSize
    then 1
    else max 0 $ min (0.4 - (maxSize' - size') * 0.1) ((size' / maxSize') ** 8)
  where
    maxSize' = fromIntegral maxSize
    size' = fromIntegral size

playoutS :: MonadRandom m => GameState -> m Double
playoutS state@(GameState board _) = 
  if maxSize == 0
    then return 1
    else calcReward maxSize <$> playoutS' state
  where
    maxSize = length $ remainingTiles board

playoutB :: MonadRandom m => Board -> m Double
playoutB board =
  if maxSize == 0
    then return 1
    else calcReward maxSize <$> playoutB' board
  where
    maxSize = length $ remainingTiles board