{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}


module Data.Tsuro.Search.Montecarlo
  ( Config (..)
  , defaultConfig
  , fastConfig
  )
where

import Control.Applicative
import Control.Arrow
import Control.Monad.Random
import Data.Either
import Data.List
import Data.Ord
import Data.Reflection
import Data.Tsuro.Core
import Data.Tsuro.Search.Class
import System.Random
import Ziphil.Util.Core
import Ziphil.Util.List
import Ziphil.Util.Random


data Config = Config {iterateSize :: Int, thresholdNum :: Int, expParam :: Double, shallownessCoeff :: Double}

instance MonadRandom m => Search m Config where
  runSearch config = give config search
  runSearchWithRatio config = give config searchWithRatio

defaultConfig :: Config
defaultConfig = Config 10000 5 3 0.05

fastConfig :: Config
fastConfig = Config 1000 4 3 0

type BoardLabel = (Board, GameMove)
type Label = Either GameState BoardLabel

data SearchTree = Node {label :: Label, num :: Int, accum :: Double, extra :: Double, children :: [SearchTree]}
  deriving (Eq, Show)

-- そのノードからの探索における報酬値の平均を返します。
ratio :: SearchTree -> Double
ratio (Node _ num accum _ _) = 
  if num == 0
    then 0
    else accum / fromIntegral num

-- 探索回数が少ないノードもある程度探索されるようにするための補正項を返します。
-- ここでは、UCT (upper confidence bound applied to tree) に基づく値を返します。
correction :: Given Config => SearchTree -> SearchTree -> Double
correction parent child = 
  if num child == 0
    then 1 / 0
    else sqrt (log parentNum / childNum) * expParam given
  where
    parentNum = fromIntegral $ num parent
    childNum = fromIntegral $ num child

score :: Given Config => SearchTree -> SearchTree -> Double
score parent child = ratio child + extra child + correction parent child

initialSearchTree :: Given Config => GameState -> SearchTree
initialSearchTree state = Node (Left state) 0 0 0 (makeChildrenS state)

-- モンテカルロ木探索を規定回数だけ実行して、与えられたゲーム状況において最適と思われる手を返します。
search :: (Given Config, MonadRandom m) => GameState -> m GameMove
search state = make <$> iterateMontecarlo (initialSearchTree state)
  where
    make = snd . fromRight undefined . label . maximumBy (comparing ratio) . children

-- モンテカルロ木探索を規定回数だけ実行して、与えられたゲーム状況において最適と思われる手に加え、その選択の確信度を返します。
-- 確信度は 0 以上 1 以下の数で、1 に近いほどその手を実行することでクリアできると確信していることを表します。
searchWithRatio :: (Given Config, MonadRandom m) => GameState -> m (GameMove, Double)
searchWithRatio state = make <$> iterateMontecarlo (initialSearchTree state)
  where
    make = (snd . fromRight undefined . label &&& ratio) . maximumBy (comparing ratio) . children

iterateMontecarlo :: (Given Config, MonadRandom m) => SearchTree -> m SearchTree
iterateMontecarlo node = iterationList !! iterateSize given
  where
    iterationList = iterate (montecarlo' =<<) $ return node

montecarlo' :: (Given Config, MonadRandom m) => SearchTree -> m SearchTree
montecarlo' = (fst <$>) . montecarlo

-- 指定されたノードからモンテカルロ木探索を 1 ステップ実行し、実行後のノードとプレイアウトの報酬値を返します。
montecarlo :: (Given Config, MonadRandom m) => SearchTree -> m (SearchTree, Double)
montecarlo node@(Node _ num _ _ children) =
  if null children
    then if num < thresholdNum given
      then montecarloPlayout node
      else montecarloExpand node
    else montecarloRecursion node

montecarloPlayout :: (Given Config, MonadRandom m) => SearchTree -> m (SearchTree, Double)
montecarloPlayout (Node label num accum extra _) = (makeNode &&& id) <$> playout label
  where
    makeNode reward = Node label (num + 1) (accum + reward) extra []

montecarloExpand :: (Given Config, MonadRandom m) => SearchTree -> m (SearchTree, Double) 
montecarloExpand node@(Node label num accum extra _) = 
  if null children
    then montecarloLeaf node
    else montecarloRecursion nextNode
  where
    nextNode = Node label num accum extra children
    children = makeChildren label

montecarloRecursion :: (Given Config, MonadRandom m) => SearchTree -> m (SearchTree, Double)
montecarloRecursion node@(Node label num accum extra children) = (makeNode &&& snd) <$> montecarlo child
  where
    makeNode (nextChild, reward) = Node label (num + 1) (accum + reward) extra (update index nextChild children)
    (index, child) = maximumBy' (comparing $ score node) children

montecarloLeaf :: (Given Config, MonadRandom m) => SearchTree -> m (SearchTree, Double)
montecarloLeaf node@(Node label num accum extra _) = return (nextNode, reward)
  where
    nextNode = Node label (num + 1) (accum + reward) extra []
    reward = either (const 0) (const 1) label

makeChildren :: Given Config => Label -> [SearchTree]
makeChildren = either makeChildrenS (makeChildrenB . fst)

outerStonePoss :: [StonePos]
outerStonePoss = (tops ++ rights ++ bottoms ++ lefts) \\ initialStones
  where
    tops = comb (map (, 0) [0 .. boardSize - 1]) [TopLeft, TopRight]
    rights = comb (map (boardSize - 1, ) [0 .. boardSize - 1]) [RightTop, RightBottom]
    bottoms = comb (map (, boardSize - 1) [0 .. boardSize - 1]) [BottomRight, BottomLeft]
    lefts = comb (map (0, ) [0 .. boardSize - 1]) [LeftBottom, LeftTop]

-- 与えられた位置が盤面の外周にどれだけ近いかを返します。
-- 具体的には、盤面の中央にある位置であれば 0 を返し、盤面の外周に向かって移動するにつれて 1 ずつ増加する整数を返します。
shallowness :: TilePos -> Int
shallowness (x, y) = (boardSize - 1) #/ 2 - min x' y'
  where
    x' = min x (boardSize - x - 1)
    y' = min y (boardSize - y - 1)

maxShallowness :: Int
maxShallowness = (boardSize - 1) #/ 2 + 1

calcExtra :: Given Config => Board -> Double
calcExtra (Board tiles _ _ _) = 
  if shallownessCoeff given == 0
    then 0
    else fromIntegral shallownessSum * shallownessCoeff given
  where
    shallownessSum = sum $ map (either (const maxShallowness) (shallowness . fst) . advanceStone tiles) outerStonePoss

makeChildrenS :: Given Config => GameState -> [SearchTree]
makeChildrenS state@(GameState board hand) = map makeNode $ possibleMovesAndBoards' state
  where
    makeNode (move, board) = Node (Right (board, move)) 0 0 (calcExtra board) []

makeChildrenB :: Board -> [SearchTree]
makeChildrenB board = map makeNode $ remainingTiles board
  where
    makeNode tile = Node (Left (GameState board tile)) 0 0 0 []

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