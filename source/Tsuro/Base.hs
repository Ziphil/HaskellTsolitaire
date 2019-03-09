{-# LANGUAGE TupleSections #-}


module Tsuro.Base where

import Control.Monad
import Data.Array
import Data.List
import Data.Maybe
import Prelude hiding (Left, Right)
import System.Random


data Rotation = Top | Right | Bottom | Left
  deriving (Eq, Enum, Show)

data Edge = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop
  deriving (Eq, Enum, Show)

rotateEdge :: Rotation -> Edge -> Edge
rotateEdge Top = id
rotateEdge Right = toEnum . flip mod 8 . (+ 2) . fromEnum
rotateEdge rotation = rotateEdge (pred rotation) . rotateEdge Right

newtype Aisles = Aisles [(Edge, Edge)]

rotateAisles :: Rotation -> Aisles -> Aisles
rotateAisles = liftAisles . map . mapPair . rotateEdge
  where
    mapPair func (s, t) = (func s, func t)
    liftAisles func (Aisles list) = Aisles (func list)

-- 与えられた番号に対応する通路情報を返します。
-- 番号は 0 以上 34 以下でなければならず、それ以外の値が渡された場合はエラーが発生します。
getAisles :: Int -> Aisles
getAisles n =
  Aisles $ case n of
    0 -> [(TopLeft, TopRight), (RightTop, RightBottom), (BottomRight, BottomLeft), (LeftBottom, LeftTop)]
    1 -> [(TopLeft, TopRight), (RightTop, RightBottom), (BottomRight, LeftBottom), (BottomLeft, LeftTop)]
    2 -> [(TopLeft, TopRight), (RightTop, RightBottom), (BottomRight, LeftTop), (BottomLeft, LeftBottom)]
    3 -> [(TopLeft, TopRight), (RightTop, BottomRight), (RightBottom, LeftBottom), (BottomLeft, LeftTop)]
    4 -> [(TopLeft, TopRight), (RightTop, BottomRight), (RightBottom, LeftTop), (BottomLeft, LeftBottom)]
    5 -> [(TopLeft, TopRight), (RightTop, BottomLeft), (RightBottom, LeftBottom), (BottomRight, LeftTop)]
    6 -> [(TopLeft, TopRight), (RightTop, BottomLeft), (RightBottom, LeftTop), (BottomRight, LeftBottom)]
    7 -> [(TopLeft, TopRight), (RightTop, LeftBottom), (RightBottom, BottomRight), (BottomLeft, LeftTop)]
    8 -> [(TopLeft, TopRight), (RightTop, LeftBottom), (RightBottom, BottomLeft), (BottomRight, LeftTop)]
    9 -> [(TopLeft, TopRight), (RightTop, LeftBottom), (RightBottom, LeftTop), (BottomRight, BottomLeft)]
    10 -> [(TopLeft, TopRight), (RightTop, LeftTop), (RightBottom, BottomRight), (BottomLeft, LeftBottom)]
    11 -> [(TopLeft, TopRight), (RightTop, LeftTop), (RightBottom, BottomLeft), (BottomRight, LeftBottom)]
    12 -> [(TopLeft, TopRight), (RightTop, LeftTop), (RightBottom, LeftBottom), (BottomRight, BottomLeft)]
    13 -> [(TopLeft, RightTop), (TopRight, RightBottom), (BottomRight, LeftBottom), (BottomLeft, LeftTop)]
    14 -> [(TopLeft, RightTop), (TopRight, RightBottom), (BottomRight, LeftTop), (BottomLeft, LeftBottom)]
    15 -> [(TopLeft, RightTop), (TopRight, BottomRight), (RightBottom, LeftBottom), (BottomLeft, LeftTop)]
    16 -> [(TopLeft, RightTop), (TopRight, BottomRight), (RightBottom, LeftTop), (BottomLeft, LeftBottom)]
    17 -> [(TopLeft, RightTop), (TopRight, BottomLeft), (RightBottom, LeftBottom), (BottomRight, LeftTop)]
    18 -> [(TopLeft, RightTop), (TopRight, BottomLeft), (RightBottom, LeftTop), (BottomRight, LeftBottom)]
    19 -> [(TopLeft, RightTop), (TopRight, LeftBottom), (RightBottom, BottomRight), (BottomLeft, LeftTop)]
    20 -> [(TopLeft, RightTop), (TopRight, LeftBottom), (RightBottom, BottomLeft), (BottomRight, LeftTop)]
    21 -> [(TopLeft, RightTop), (TopRight, LeftTop), (RightBottom, BottomRight), (BottomLeft, LeftBottom)]
    22 -> [(TopLeft, RightTop), (TopRight, LeftTop), (RightBottom, BottomLeft), (BottomRight, LeftBottom)]
    23 -> [(TopLeft, RightBottom), (TopRight, RightTop), (BottomRight, LeftTop), (BottomLeft, LeftBottom)]
    24 -> [(TopLeft, RightBottom), (TopRight, BottomRight), (RightTop, LeftBottom), (BottomLeft, LeftTop)]
    25 -> [(TopLeft, RightBottom), (TopRight, BottomRight), (RightTop, LeftTop), (BottomLeft, LeftBottom)]
    26 -> [(TopLeft, RightBottom), (TopRight, BottomLeft), (RightTop, LeftBottom), (BottomRight, LeftTop)]
    27 -> [(TopLeft, RightBottom), (TopRight, LeftBottom), (RightTop, BottomLeft), (BottomRight, LeftTop)]
    28 -> [(TopLeft, BottomRight), (TopRight, RightTop), (RightBottom, LeftBottom), (BottomLeft, LeftTop)]
    29 -> [(TopLeft, BottomRight), (TopRight, RightTop), (RightBottom, LeftTop), (BottomLeft, LeftBottom)]
    30 -> [(TopLeft, BottomRight), (TopRight, RightBottom), (RightTop, LeftBottom), (BottomLeft, LeftTop)]
    31 -> [(TopLeft, BottomRight), (TopRight, BottomLeft), (RightTop, LeftBottom), (RightBottom, LeftTop)]
    32 -> [(TopLeft, BottomRight), (TopRight, BottomLeft), (RightTop, LeftTop), (RightBottom, LeftBottom)]
    33 -> [(TopLeft, BottomLeft), (TopRight, BottomRight), (RightTop, LeftTop), (RightBottom, LeftBottom)]
    34 -> [(TopLeft, LeftTop), (TopRight, RightTop), (RightBottom, BottomRight), (BottomLeft, LeftBottom)]
    _ -> error "Invalid"

data Tile = Tile {number :: Int, rotation :: Rotation}
  deriving (Eq, Show)

aislesOf :: Tile -> Aisles
aislesOf (Tile number rotation) = rotateAisles rotation (getAisles number)

type TilePos = (Int, Int)
type StonePos = (TilePos, Edge)

boardSize :: Int
boardSize = 6

-- 指定した方向に隣接する位置を返します。
-- 指定した方向が盤面外の場合は、Nothing を返します。
adjacent :: Rotation -> TilePos -> Maybe TilePos
adjacent Top (x, y) = guard (y > 0) >> Just (x, y - 1)
adjacent Right (x, y) = guard (x < boardSize - 1) >> Just (x + 1, y)
adjacent Bottom (x, y) = guard (y < boardSize - 1) >> Just (x, y + 1)
adjacent Left (x, y) = guard (x > 0) >> Just (x - 1, y)

newtype Tiles = Tiles (Array TilePos (Maybe Tile))
  deriving (Eq, Show)

-- 空の盤面を返します。
emptyTiles :: Tiles
emptyTiles = Tiles $ array bounds $ map (, Nothing) (range bounds)
  where
    bounds = ((0, 0), (boardSize - 1, boardSize - 1))

-- 見た目上で同じ場所を表すもう一方の駒位置を返します。
-- 例えば、横に隣り合う 2 つのマスの間の上側は、左側のマスから見て RightTop の位置ですが、右側のマスから見て LeftTop の位置でもあります。
-- このように、見た目では同じ場所でも駒位置としては 2 種類の表現があり、この関数は自身とは別のもう一方の表現を返します。
-- 駒位置が盤面の縁を表している場合は、駒位置の表現は 1 種類しかあり得ないため、Nothing を返します。
switch :: StonePos -> Maybe StonePos
switch (pos, edge) =
  liftMaybe $ make $ case edge of
    TopLeft -> (Top, BottomLeft)
    TopRight -> (Top, BottomRight)
    RightTop -> (Right, LeftTop)
    RightBottom -> (Right, LeftBottom)
    BottomRight -> (Bottom, TopRight)
    BottomLeft -> (Bottom, TopLeft)
    LeftBottom -> (Left, RightBottom)
    LeftTop -> (Left, RightTop)
  where
    make (direction, edge) = (adjacent direction pos, edge)
    liftMaybe (Just s, t) = Just (s, t)
    liftMaybe (Nothing, _) = Nothing
    
-- 端から通路情報を辿ることで到達する反対側の端を返します。
-- 通路情報が十分ない (8 ヶ所の端のうち別の端と繋がっていない端が存在する) 場合、エラーが発生します。
opposite :: Edge -> Aisles -> Edge
opposite edge (Aisles aisles) = head $ mapMaybe choose aisles
  where
    choose (startEdge, endEdge)
      | startEdge == edge = Just endEdge
      | endEdge == edge = Just startEdge
      | otherwise = Nothing

-- 指定された位置のタイルを指定されたタイルで置き換え、その結果の盤面を返します。
updateTile :: TilePos -> Tile -> Tiles -> Tiles
updateTile tilePos tile (Tiles tiles) = Tiles $ tiles // [(tilePos, Just tile)]

-- 盤面に置かれているタイルの通路に沿って、与えられた駒位置から可能な限り進んだときに到達する駒位置を返します。
-- 進む途中で盤面外に出てしまう場合は、Nothing を返します。
advanceStone :: Tiles -> StonePos -> Maybe StonePos
advanceStone (Tiles tiles) (tilePos, edge) =
  case tiles ! tilePos of
    Nothing -> Just (tilePos, edge)
    Just tile -> advanceStone (Tiles tiles) =<< nextStonePos
      where
        nextStonePos = switch (tilePos, nextEdge)
        nextEdge = opposite edge (aislesOf tile)

data Board = Board {tiles :: Tiles, stones :: [StonePos]}
  deriving (Eq, Show)

-- 駒の初期位置を返します。
initialStones :: [StonePos]
initialStones = [((1, 0), TopRight), ((4, 0), TopLeft), ((5, 1), RightBottom), ((5, 4), RightTop), ((4, 5), BottomLeft), ((1, 5), BottomRight), ((0, 4), LeftTop), ((0, 1), LeftBottom)]

-- 初期状態の盤面を返します。
initialBoard :: Board
initialBoard = Board emptyTiles initialStones

-- 指定された位置にタイルが置かれていないか確かめ、置かれていなければ True を返します。
isEmpty :: TilePos -> Board -> Bool
isEmpty tilePos (Board (Tiles tiles) _) = isNothing (tiles ! tilePos)

-- 指定された位置が何らかの駒と隣接しているかどうか確かめ、隣接していれば True を返します。
-- この関数が False を返すような位置には、ルール上タイルを置くことができません。
isAdjacentStone :: TilePos -> Board -> Bool
isAdjacentStone tilePos (Board _ stones) = any check stones
  where
    check (eachTilePos, _) = eachTilePos == tilePos

-- 駒の移動を考えないとき、指定された位置にタイルを置くことができるか確かめ、置けるならば True を返します。
-- ただし、タイルを置いた後に駒が盤面外に出てしまうかどうかは考慮しません。
canPutTile' :: TilePos -> Board -> Bool
canPutTile' tilePos board = isEmpty tilePos board && isAdjacentStone tilePos board

-- タイルを指定された位置に置いた後の盤面を返します。
-- 指定された位置にすでにタイルが置かれている場合は、新たにタイルを置くことはできないので、Nothing を返します。
-- この関数単独では駒を動かしません。
putTile :: TilePos -> Tile -> Board -> Maybe Board
putTile tilePos tile board =
  if canPutTile' tilePos board
    then Just $ Board nextTiles (stones board)
    else Nothing
  where
    nextTiles = updateTile tilePos tile (tiles board)

-- 現在の盤面に従って全ての駒を移動させ、その結果の盤面を返します。
-- 進む途中で盤面外に出てしまうような駒が 1 つでもある場合は、Nothing を返します。
advanceStones :: Board -> Maybe Board
advanceStones (Board tiles stones) = 
  case mapM (advanceStone tiles) stones of
    Nothing -> Nothing
    Just nextStones -> Just $ Board tiles nextStones

canAdvenceStones :: Board -> Bool
canAdvenceStones = isJust . advanceStones

-- タイルを指定された位置に置き、さらにその後の盤面に従って全ての駒を移動させ、その結果の盤面を返します。
-- 不可能な操作をしようとした場合は、Nothing を返します。
putTileAndUpdate :: TilePos -> Tile -> Board -> Maybe Board
putTileAndUpdate tilePos tile = advanceStones <=< putTile tilePos tile

-- 指定された位置にタイルを置くことができるか確かめ、置けるならば True を返します。
canPutTile :: TilePos -> Tile -> Board -> Bool
canPutTile = ((isJust .) .) . putTileAndUpdate

-- 指定された手を実行した後の盤面を返します。
-- 不可能な操作をしようとした場合は、Nothing を返します。
infixl 6 <<~
(<<~) :: Board -> (TilePos, Tile) -> Maybe Board
(<<~) = flip $ uncurry putTileAndUpdate

-- 指定された手を実行した後の盤面を返します。
-- 不可能な操作をしようとした場合は、エラーが発生します。
infixl 6 <!~
(<!~) :: Board -> (TilePos, Tile) -> Board
(<!~) = (fromJust .) . (<<~)

data Game = Game {board :: Board, hands :: [Tile]}
  deriving (Eq, Show)

nubRandomRs :: RandomGen g => (Int, Int) -> g -> [Int]
nubRandomRs (low, high) gen = take (high - low + 1) $ nub $ randomRs (low, high) gen

-- 初期状態の残りタイルをシャッフルしない状態で返します。
initialHands' :: [Tile]
initialHands' = map (flip Tile Top) [0 .. 34]

-- 初期状態の残りタイルを返します。
initialHands :: RandomGen g => g -> [Tile]
initialHands gen = map fst $ sortOn snd $ zip initialHands' randoms
  where
    randoms = nubRandomRs (0, 34) gen

-- 初期状態のゲームをシャッフルしない状態で返します。
initialGame' :: Game
initialGame' = Game initialBoard initialHands'

-- 初期状態のゲームを返します。
initialGame :: RandomGen g => g -> Game
initialGame gen = Game initialBoard (initialHands gen)

-- 次に置くべきタイルを返します。
-- 全てのタイルを置き切っていて置くべきタイルが残っていない場合は、Nothing を返します。
nextHand :: Game -> Maybe Tile
nextHand = undefined

-- 指定された位置に置くべきタイルを置きます。
-- 不可能な操作をしようとした場合は、Nothing を返します。
move :: TilePos -> Game -> Maybe Game
move = undefined