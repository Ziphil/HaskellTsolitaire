{-# LANGUAGE TupleSections #-}


module Tsuro.Base where

import Control.Monad
import Data.Array
import Data.Either
import Data.List
import Data.Maybe
import System.Random
import ZiphilUtil
import ZiphilUtil.Random


data Rotation = None | Clock | Inverse | Anticlock
  deriving (Eq, Enum, Show)

data Edge = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop
  deriving (Eq, Enum, Show)

rotateEdge :: Rotation -> Edge -> Edge
rotateEdge None = id
rotateEdge Clock = toEnum . flip mod 8 . (+ 2) . fromEnum
rotateEdge rotation = rotateEdge (pred rotation) . rotateEdge Clock

newtype Aisles = Aisles {aisleList :: [(Edge, Edge)]}

rotateAisles :: Rotation -> Aisles -> Aisles
rotateAisles = outAisles . map . bimapSame . rotateEdge
  where
    outAisles func (Aisles list) = Aisles (func list)

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
    _ -> error "invalid tile number"

data Tile = Tile {number :: Int, rotation :: Rotation}
  deriving (Eq, Show)

aislesOf :: Tile -> Aisles
aislesOf (Tile number rotation) = rotateAisles rotation (getAisles number)

type TilePos = (Int, Int)
type StonePos = (TilePos, Edge)

data InvalidMove = OutOfBoard | TileAlreadyPut | DetachedTilePos | NoNextHand
  deriving (Eq, Show)

type TsuroMaybe = Either InvalidMove

boardSize :: Int
boardSize = 6

tileSize :: Int
tileSize = 35

-- 指定した方向に隣接する位置を返します。
-- 指定した方向が盤面外の場合は、OutOfBoard を返します。
adjacent :: Rotation -> TilePos -> TsuroMaybe TilePos
adjacent direction (x, y) =
  make $ case direction of
    None -> (y > 0, (x, y - 1))
    Clock -> (x < boardSize - 1, (x + 1, y))
    Inverse -> (y < boardSize - 1, (x, y + 1))
    Anticlock -> (x > 0, (x - 1, y))
  where
    make (pred, pos) = unless pred (Left OutOfBoard) >> Right pos

newtype Tiles = Tiles {tileList :: Array TilePos (Maybe Tile)}
  deriving (Eq, Show)

-- 空の盤面を返します。
emptyTiles :: Tiles
emptyTiles = Tiles $ array bounds $ map (, Nothing) (range bounds)
  where
    bounds = ((0, 0), (boardSize - 1, boardSize - 1))

-- 見た目上で同じ場所を表すもう一方の駒位置を返します。
-- 例えば、横に隣り合う 2 つのマスの間の上側は、左側のマスから見て RightTop の位置ですが、右側のマスから見て LeftTop の位置でもあります。
-- このように、見た目では同じ場所でも駒位置としては 2 種類の表現があり、この関数は自身とは別のもう一方の表現を返します。
-- 駒位置が盤面の縁を表している場合は、駒位置の表現は 1 種類しかあり得ないため、OutOfBoard を返します。
switch :: StonePos -> TsuroMaybe StonePos
switch (pos, edge) =
  outFstEither $ make $ case edge of
    TopLeft -> (None, BottomLeft)
    TopRight -> (None, BottomRight)
    RightTop -> (Clock, LeftTop)
    RightBottom -> (Clock, LeftBottom)
    BottomRight -> (Inverse, TopRight)
    BottomLeft -> (Inverse, TopLeft)
    LeftBottom -> (Anticlock, RightBottom)
    LeftTop -> (Anticlock, RightTop)
  where
    make (direction, edge) = (adjacent direction pos, edge)
    
-- 端から通路情報を辿ることで到達する反対側の端を返します。
-- 通路情報が十分ない (8 ヶ所の端のうち別の端と繋がっていない端が存在する) 場合、エラーが発生します。
opposite :: Edge -> Aisles -> Edge
opposite edge (Aisles aisleList) = head $ mapMaybe choose aisleList
  where
    choose (startEdge, endEdge)
      | startEdge == edge = Just endEdge
      | endEdge == edge = Just startEdge
      | otherwise = Nothing

-- 指定された位置のタイルを指定されたタイルで置き換え、その結果の盤面を返します。
updateTile :: TilePos -> Tile -> Tiles -> Tiles
updateTile pos tile (Tiles tileList) = Tiles $ tileList // [(pos, Just tile)]

-- 盤面に置かれているタイルの通路に沿って、与えられた駒位置から可能な限り進んだときに到達する駒位置を返します。
-- 進む途中で盤面外に出てしまう場合は、OutOfBoard を返します。
advanceStone :: Tiles -> StonePos -> TsuroMaybe StonePos
advanceStone (Tiles tileList) (tilePos, edge) =
  case tileList ! tilePos of
    Nothing -> Right (tilePos, edge)
    Just tile -> advanceStone (Tiles tileList) =<< nextStonePos
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
isEmpty tilePos (Board (Tiles tileList) _) = isNothing (tileList ! tilePos)

-- 指定された位置が何らかの駒と隣接しているかどうか確かめ、隣接していれば True を返します。
-- この関数が False を返すような位置には、ルール上タイルを置くことができません。
isAdjacentStone :: TilePos -> Board -> Bool
isAdjacentStone pos (Board _ stones) = any check stones
  where
    check = (== pos) . fst 

type TileMove = (TilePos, Tile)

-- タイルを指定された位置に置いた後の盤面を返します。
-- 指定された位置にすでにタイルが置かれている場合は、新たにタイルを置くことはできないので、TileAlreadyPut を返します。
-- また、指定された位置が何らかの駒と隣接していない場合は、ルール上その位置にタイルを置くことはできないので、DetachedTilePos を返します。
-- この関数単独では駒を動かしません。
putTile :: TileMove -> Board -> TsuroMaybe Board
putTile (pos, tile) board =
  unless isEmpty' (Left TileAlreadyPut) >> unless isAdjacentStone' (Left DetachedTilePos) >> Right nextBoard
    where
      isEmpty' = isEmpty pos board
      isAdjacentStone' = isAdjacentStone pos board
      nextBoard = Board (updateTile pos tile (tiles board)) (stones board)

-- 現在の盤面に従って全ての駒を移動させ、その結果の盤面を返します。
-- 進む途中で盤面外に出てしまうような駒が 1 つでもある場合は、OutOfBoard を返します。
advanceStones :: Board -> TsuroMaybe Board
advanceStones (Board tiles stones) = Board tiles <$> mapM (advanceStone tiles) stones

canAdvanceStones :: Board -> Bool
canAdvanceStones = isRight . advanceStones

-- タイルを指定された位置に置き、さらにその後の盤面に従って全ての駒を移動させ、その結果の盤面を返します。
-- 不可能な操作をしようとした場合は、その原因を示すエラー値を返します。
putTileAndUpdate :: TileMove -> Board -> TsuroMaybe Board
putTileAndUpdate move = advanceStones <=< putTile move

-- 指定された位置にタイルを置くことができるか確かめ、置けるならば True を返します。
canPutTile :: TileMove -> Board -> Bool
canPutTile = (isRight .) . putTileAndUpdate

-- 指定されたタイルを置ける位置が存在するか確かめ、存在するならば True を返します。
canPutTileAnywhere :: Tile -> Board -> Bool
canPutTileAnywhere tile board = any check (indices $ tileList $ tiles board)
  where
    check pos = canPutTile (pos, tile) board

data Game = Game {board :: Board, hands :: [Tile]}
  deriving (Eq, Show)

-- 初期状態の残りタイルをシャッフルしない状態で返します。
initialHands' :: [Tile]
initialHands' = map (flip Tile None) [0 .. tileSize - 1]

-- 初期状態の残りタイルを返します。
initialHands :: RandomGen g => g -> [Tile]
initialHands = map fst . sortOn snd . zip initialHands' . nubRandomRs (0, tileSize - 1)

-- 初期状態のゲームをシャッフルしない状態で返します。
initialGame' :: Game
initialGame' = Game initialBoard initialHands'

-- 初期状態のゲームを返します。
initialGame :: RandomGen g => g -> Game
initialGame = Game initialBoard . initialHands

-- 与えられたタイル番号のリストの順番で手札が出てくるような初期状態のゲームを返します。
-- タイル番号のリストは、0 以上 34 以下の整数が重複なく過不足なく出現している必要があります。
-- この関数はデバッグ用です。 
createGame :: [Int] -> Game
createGame numbers =
  if isPermutation numbers [0 .. tileSize - 1]
    then Game initialBoard (map (flip Tile None) numbers)
    else error "invalid tile numbers"

-- 次に置くべきタイルを返します。
-- 全てのタイルを置き切っていて置くべきタイルが残っていない場合は、NoNextHand を返します。
nextHand :: Game -> TsuroMaybe Tile
nextHand (Game _ []) = Left NoNextHand
nextHand (Game _ (hand : _)) = Right hand

restHands :: Game -> TsuroMaybe [Tile]
restHands (Game _ []) = Left NoNextHand
restHands (Game _ (_ : rest)) = Right rest

rotateTile :: Rotation -> Tile -> Tile
rotateTile rotation (Tile number _) = Tile number rotation

type GameMove = (TilePos, Rotation)

-- 指定された位置に置くべきタイルを置きます。
-- 不可能な操作をしようとした場合は、その原因を示すエラー値を返します。
applyMove :: GameMove -> Game -> TsuroMaybe Game
applyMove (pos, rotation) game = makeGame =<< putTileAndUpdate' =<< nextHand game
  where
    putTileAndUpdate' tile = putTileAndUpdate (pos, rotateTile rotation tile) (board game)
    makeGame board = Game board <$> restHands game

-- ゲームをクリアしていれば True を返します。
isCleared :: Game -> Bool
isCleared (Game _ hands) = null hands

-- 次に置くべきタイルを置ける場所がなく、これ以上ゲームを進められない場合に、True を返します。
isOver :: Game -> Bool
isOver game = either (const False) check $ nextHand game
  where
    check = not . flip canPutTileAnywhere (board game)