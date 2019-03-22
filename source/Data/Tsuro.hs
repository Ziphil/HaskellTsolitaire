{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}


module Data.Tsuro where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Data.Array.IArray
import Data.Either
import Data.Function
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple
import System.Random
import Ziphil.Util.Core
import Ziphil.Util.List
import Ziphil.Util.Monad
import Ziphil.Util.Random


data Rotation = None | Clock | Inverse | Anticlock
  deriving (Eq, Ord, Ix, Enum, Show)

data Edge = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop
  deriving (Eq, Ord, Ix, Enum, Show)

rotateEdge :: Rotation -> Edge -> Edge
rotateEdge None = id
rotateEdge Clock = toEnum . (#% 8) . (+ 2) . fromEnum
rotateEdge rotation = rotateEdge (pred rotation) . rotateEdge Clock

newtype Aisles = Aisles {rawAisles :: Set (Edge, Edge)}
  deriving (Eq, Show)

type TileInfo = (Int, Rotation)

rotateAisles :: Rotation -> Aisles -> Aisles
rotateAisles = outAisles . Set.map . bimapSame . rotateEdge
  where
    outAisles func (Aisles set) = Aisles (func set)

aisleArray :: Array TileInfo Aisles
aisleArray = array ((0, None), (34, Anticlock)) $ map make $ comb dataList allEnums
  where
    make ((number, list), rotation) = ((number, rotation), rotateAisles rotation $ makeAisles list)
    makeAisles = Aisles . Set.fromList . concatMap (take 2 . iterate swap)
    dataList =
      [ (0, [(TopLeft, TopRight), (RightTop, RightBottom), (BottomRight, BottomLeft), (LeftBottom, LeftTop)])
      , (1, [(TopLeft, TopRight), (RightTop, RightBottom), (BottomRight, LeftBottom), (BottomLeft, LeftTop)])
      , (2, [(TopLeft, TopRight), (RightTop, RightBottom), (BottomRight, LeftTop), (BottomLeft, LeftBottom)])
      , (3, [(TopLeft, TopRight), (RightTop, BottomRight), (RightBottom, LeftBottom), (BottomLeft, LeftTop)])
      , (4, [(TopLeft, TopRight), (RightTop, BottomRight), (RightBottom, LeftTop), (BottomLeft, LeftBottom)])
      , (5, [(TopLeft, TopRight), (RightTop, BottomLeft), (RightBottom, LeftBottom), (BottomRight, LeftTop)])
      , (6, [(TopLeft, TopRight), (RightTop, BottomLeft), (RightBottom, LeftTop), (BottomRight, LeftBottom)])
      , (7, [(TopLeft, TopRight), (RightTop, LeftBottom), (RightBottom, BottomRight), (BottomLeft, LeftTop)])
      , (8, [(TopLeft, TopRight), (RightTop, LeftBottom), (RightBottom, BottomLeft), (BottomRight, LeftTop)])
      , (9, [(TopLeft, TopRight), (RightTop, LeftBottom), (RightBottom, LeftTop), (BottomRight, BottomLeft)])
      , (10, [(TopLeft, TopRight), (RightTop, LeftTop), (RightBottom, BottomRight), (BottomLeft, LeftBottom)])
      , (11, [(TopLeft, TopRight), (RightTop, LeftTop), (RightBottom, BottomLeft), (BottomRight, LeftBottom)])
      , (12, [(TopLeft, TopRight), (RightTop, LeftTop), (RightBottom, LeftBottom), (BottomRight, BottomLeft)])
      , (13, [(TopLeft, RightTop), (TopRight, RightBottom), (BottomRight, LeftBottom), (BottomLeft, LeftTop)])
      , (14, [(TopLeft, RightTop), (TopRight, RightBottom), (BottomRight, LeftTop), (BottomLeft, LeftBottom)])
      , (15, [(TopLeft, RightTop), (TopRight, BottomRight), (RightBottom, LeftBottom), (BottomLeft, LeftTop)])
      , (16, [(TopLeft, RightTop), (TopRight, BottomRight), (RightBottom, LeftTop), (BottomLeft, LeftBottom)])
      , (17, [(TopLeft, RightTop), (TopRight, BottomLeft), (RightBottom, LeftBottom), (BottomRight, LeftTop)])
      , (18, [(TopLeft, RightTop), (TopRight, BottomLeft), (RightBottom, LeftTop), (BottomRight, LeftBottom)])
      , (19, [(TopLeft, RightTop), (TopRight, LeftBottom), (RightBottom, BottomRight), (BottomLeft, LeftTop)])
      , (20, [(TopLeft, RightTop), (TopRight, LeftBottom), (RightBottom, BottomLeft), (BottomRight, LeftTop)])
      , (21, [(TopLeft, RightTop), (TopRight, LeftTop), (RightBottom, BottomRight), (BottomLeft, LeftBottom)])
      , (22, [(TopLeft, RightTop), (TopRight, LeftTop), (RightBottom, BottomLeft), (BottomRight, LeftBottom)])
      , (23, [(TopLeft, RightBottom), (TopRight, RightTop), (BottomRight, LeftTop), (BottomLeft, LeftBottom)])
      , (24, [(TopLeft, RightBottom), (TopRight, BottomRight), (RightTop, LeftBottom), (BottomLeft, LeftTop)])
      , (25, [(TopLeft, RightBottom), (TopRight, BottomRight), (RightTop, LeftTop), (BottomLeft, LeftBottom)])
      , (26, [(TopLeft, RightBottom), (TopRight, BottomLeft), (RightTop, LeftBottom), (BottomRight, LeftTop)])
      , (27, [(TopLeft, RightBottom), (TopRight, LeftBottom), (RightTop, BottomLeft), (BottomRight, LeftTop)])
      , (28, [(TopLeft, BottomRight), (TopRight, RightTop), (RightBottom, LeftBottom), (BottomLeft, LeftTop)])
      , (29, [(TopLeft, BottomRight), (TopRight, RightTop), (RightBottom, LeftTop), (BottomLeft, LeftBottom)])
      , (30, [(TopLeft, BottomRight), (TopRight, RightBottom), (RightTop, LeftBottom), (BottomLeft, LeftTop)])
      , (31, [(TopLeft, BottomRight), (TopRight, BottomLeft), (RightTop, LeftBottom), (RightBottom, LeftTop)])
      , (32, [(TopLeft, BottomRight), (TopRight, BottomLeft), (RightTop, LeftTop), (RightBottom, LeftBottom)])
      , (33, [(TopLeft, BottomLeft), (TopRight, BottomRight), (RightTop, LeftTop), (RightBottom, LeftBottom)])
      , (34, [(TopLeft, LeftTop), (TopRight, RightTop), (RightBottom, BottomRight), (BottomLeft, LeftBottom)])
      ]

data Symmetry = Asymmetric | Dyad | Tetrad
  deriving (Eq, Show)

calcSymmetry :: Aisles -> Symmetry
calcSymmetry aisles = maybe Asymmetric snd $ find (check . fst) [(Clock, Tetrad), (Inverse, Dyad)]
  where
    check = (== aisles) . flip rotateAisles aisles

symmetryArray :: Array Int Symmetry
symmetryArray = array (0, tileSize - 1) $ map make [0 .. tileSize - 1]
  where
    make number = (number, calcSymmetry $ aisleArray ! (number, None))

data Tile = Tile {number :: Int, rotation :: Rotation}
  deriving (Eq, Show)

tileSize :: Int
tileSize = 35

wholeTiles :: [Tile]
wholeTiles = map (flip Tile None) [0 .. tileSize - 1]

aislesOf :: Tile -> Aisles
aislesOf (Tile number rotation) = aisleArray ! (number, rotation)

symmetryOf :: Tile -> Symmetry
symmetryOf (Tile number _) = symmetryArray ! number

-- 通路の対称性によって回転が異なっていても見た目が同じになるタイルに対し、回転情報を正規化したタイルを返します。
-- 具体的には、以下のような動作をします。
-- 90° 回転でもとに戻るタイルの場合、回転を全て None にします。
-- 90° 回転ではもとに戻らず 180° 回転でもとに戻るタイルの場合、回転を None か Clock にします。
normalize :: Tile -> Tile
normalize tile@(Tile number rotation) = 
  case symmetryOf tile of
    Asymmetric -> tile
    Dyad -> Tile number $ toEnum $ (#% 2) $ fromEnum rotation
    Tetrad -> Tile number None

-- タイルに書かれた通路の対称性を考慮に入れ、見た目が同じになる回転情報は片方のみが含まれるようにして、回転情報全体のリストを返します。
distinctRotations :: Tile -> [Rotation]
distinctRotations tile = 
  case symmetryOf tile of
    Asymmetric -> [None, Clock, Inverse, Anticlock]
    Dyad -> [None, Clock]
    Tetrad -> [None]

-- タイルに書かれた通路の対称性を考慮に入れ、与えられたタイルを回転して得られる全てのタイルから成るリストを返します。
rotatedTiles :: Tile -> [Tile]
rotatedTiles tile@(Tile number _) = map (Tile number) $ distinctRotations tile

type TilePos = (Int, Int)
type StonePos = (TilePos, Edge)

data InvalidKind = OutOfBoard | TileAlreadyPut | DetachedTilePos | NoNextHand
  deriving (Eq, Show)

type WithInvalid = Either InvalidKind

boardSize :: Int
boardSize = 6

-- 指定した方向に隣接する位置を返します。
-- 指定した方向が盤面外の場合は、OutOfBoard を返します。
adjacent :: Rotation -> TilePos -> WithInvalid TilePos
adjacent direction (x, y) =
  make $ case direction of
    None -> (y > 0, (x, y - 1))
    Clock -> (x < boardSize - 1, (x + 1, y))
    Inverse -> (y < boardSize - 1, (x, y + 1))
    Anticlock -> (x > 0, (x - 1, y))
  where
    make (pred, pos) = unless pred (Left OutOfBoard) >> Right pos

newtype Tiles = Tiles {rawTiles :: Array TilePos (Maybe Tile)}
  deriving (Eq, Show)

tilePosBounds :: (TilePos, TilePos)
tilePosBounds = ((0, 0), (boardSize - 1, boardSize - 1))

wholeTilePoss :: [TilePos]
wholeTilePoss = range tilePosBounds

-- 空の盤面を返します。
emptyTiles :: Tiles
emptyTiles = Tiles $ array tilePosBounds $ map (, Nothing) wholeTilePoss

-- 見た目上で同じ場所を表すもう一方の駒位置を返します。
-- 例えば、横に隣り合う 2 つのマスの間の上側は、左側のマスから見て RightTop の位置ですが、右側のマスから見て LeftTop の位置でもあります。
-- このように、見た目では同じ場所でも駒位置としては 2 種類の表現があり、この関数は自身とは別のもう一方の表現を返します。
-- 駒位置が盤面の縁を表している場合は、駒位置の表現は 1 種類しかあり得ないため、OutOfBoard を返します。
switch :: StonePos -> WithInvalid StonePos
switch (pos, edge) =
  outFstA $ make $ case edge of
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

calcOpposite :: (TileInfo, Edge) -> Edge
calcOpposite (info, edge) = snd $ fromJust $ find ((== edge) . fst) (rawAisles $ aisleArray ! info)

oppositeArray :: Array (TileInfo, Edge) Edge
oppositeArray = array bounds $ map make $ comb (comb [0 .. tileSize - 1] allEnums) allEnums
  where
    make info = (info, calcOpposite info)
    bounds = (((0, None), TopLeft), ((tileSize - 1, Anticlock), LeftTop))
    
-- 端から通路情報を辿ることで到達する反対側の端を返します。
-- 通路情報が十分ない (8 ヶ所の端のうち別の端と繋がっていない端が存在する) 場合、エラーが発生します。
opposite :: Tile -> Edge -> Edge
opposite (Tile number rotation) edge = oppositeArray ! ((number, rotation), edge)

-- 指定された位置のタイルを指定されたタイルで置き換え、その結果の盤面を返します。
updateTile :: TilePos -> Tile -> Tiles -> Tiles
updateTile pos tile (Tiles tiles) = Tiles $ tiles // [(pos, Just tile)]

-- 盤面に置かれているタイルの通路に沿って、与えられた駒位置から可能な限り進んだときに到達する駒位置を返します。
-- 進む途中で盤面外に出てしまう場合は、OutOfBoard を返します。
advanceStone :: Tiles -> StonePos -> WithInvalid StonePos
advanceStone (Tiles tiles) (tilePos, edge) =
  case tiles ! tilePos of
    Nothing -> Right (tilePos, edge)
    Just tile -> advanceStone (Tiles tiles) =<< nextStonePos
      where
        nextStonePos = switch (tilePos, nextEdge)
        nextEdge = opposite tile edge

data Board = Board {tiles :: Tiles, remainingTiles :: [Tile], adjacentPoss :: [TilePos], stones :: [StonePos]}
  deriving (Eq, Show)

-- 駒の初期位置を返します。
initialStones :: [StonePos]
initialStones = [((1, 0), TopRight), ((4, 0), TopLeft), ((5, 1), RightBottom), ((5, 4), RightTop), ((4, 5), BottomLeft), ((1, 5), BottomRight), ((0, 4), LeftTop), ((0, 1), LeftBottom)]

-- 初期状態の盤面を返します。
initialBoard :: Board
initialBoard = Board emptyTiles wholeTiles adjacentPoss initialStones
  where
    adjacentPoss = filter (flip isAdjacent' initialStones) wholeTilePoss

-- 指定された位置にタイルが置かれていないか確かめ、置かれていなければ True を返します。
isEmpty :: TilePos -> Board -> Bool
isEmpty tilePos (Board (Tiles tiles) _ _ _) = isNothing (tiles ! tilePos)

isAdjacent' :: TilePos -> [StonePos] -> Bool
isAdjacent' pos = any ((== pos) . fst)

-- 指定された位置が何らかの駒と隣接しているかどうか確かめ、隣接していれば True を返します。
-- この関数が False を返すような位置には、ルール上タイルを置くことができません。
isAdjacent :: TilePos -> Board -> Bool
isAdjacent pos (Board _ _ _ stones) = isAdjacent' pos stones

-- 盤面に使われているタイルのリストを返します。
usedTiles :: Board -> [Tile]
usedTiles (Board (Tiles tiles) _ _ _) = catMaybes $ elems tiles

type TileMove = (TilePos, Tile)

-- タイルを指定された位置に置いた後の盤面を返します。
-- 指定された位置にすでにタイルが置かれている場合は、新たにタイルを置くことはできないので、TileAlreadyPut を返します。
-- また、指定された位置が何らかの駒と隣接していない場合は、ルール上その位置にタイルを置くことはできないので、DetachedTilePos を返します。
-- この関数単独では駒を動かしません。
-- そのため、駒に隣接しているタイルのリストも更新しません。
putTileOnly :: TileMove -> Board -> WithInvalid Board
putTileOnly move@(pos, _) board = unless isPosEmpty (Left TileAlreadyPut) >> unless isPosAdjacent (Left DetachedTilePos) >> putTileOnlyWithoutCheck move board
  where
    isPosEmpty = isEmpty pos board
    isPosAdjacent = isAdjacent pos board

putTileOnlyWithoutCheck :: TileMove -> Board -> WithInvalid Board
putTileOnlyWithoutCheck (pos, tile) (Board tiles remainingTiles adjacentPoss stones) = Right nextBoard
  where
    nextBoard = Board nextTiles nextRemainingTiles adjacentPoss stones
    nextTiles = updateTile pos tile tiles
    nextRemainingTiles = deleteBy (on (==) number) tile remainingTiles

-- 現在の盤面に従って全ての駒を移動させ、その結果の盤面を返します。
-- 進む途中で盤面外に出てしまうような駒が 1 つでもある場合は、OutOfBoard を返します。
advanceStones :: Board -> WithInvalid Board
advanceStones (Board tiles remainingTiles _ stones) = make <$> mapM (advanceStone tiles) stones
  where
    make stones = Board tiles remainingTiles (filter (flip isAdjacent' stones) wholeTilePoss) stones

canAdvanceStones :: Board -> Bool
canAdvanceStones = isRight . advanceStones

-- タイルを指定された位置に置き、さらにその後の盤面に従って全ての駒を移動させ、その結果の盤面を返します。
-- 不可能な操作をしようとした場合は、その原因を示すエラー値を返します。
putTile :: TileMove -> Board -> WithInvalid Board
putTile move = advanceStones <=< putTileOnly move

putTileWithoutCheck :: TileMove -> Board -> WithInvalid Board
putTileWithoutCheck move = advanceStones <=< putTileOnlyWithoutCheck move

-- 指定された位置にタイルを置くことができるか確かめ、置けるならば True を返します。
canPutTile :: TileMove -> Board -> Bool
canPutTile = isRight .^ putTile

canPutTileWithoutCheck :: TileMove -> Board -> Bool
canPutTileWithoutCheck = isRight .^ putTileWithoutCheck

-- 指定されたタイルを置ける位置が存在するか確かめ、存在するならば True を返します。
canPutTileAnywhere :: Tile -> Board -> Bool
canPutTileAnywhere tile board = any (flip canPutTileWithoutCheck board . (, tile)) $ adjacentPoss board

-- 与えられたタイルを置ける位置のリストを返します。
possiblePoss :: Tile -> Board -> [TilePos]
possiblePoss tile board = filter (flip canPutTileWithoutCheck board . (, tile)) $ adjacentPoss board

data Game = Game {board :: Board, hands :: [Tile]}
  deriving (Eq, Show)

-- 初期状態の残りタイルをシャッフルしない状態で返します。
initialHands' :: [Tile]
initialHands' = wholeTiles

-- 初期状態の残りタイルを返します。
initialHands :: MonadRandom m => m [Tile]
initialHands = shuffle initialHands'

-- 初期状態のゲームをシャッフルしない状態で返します。
initialGame' :: Game
initialGame' = Game initialBoard initialHands'

-- 初期状態のゲームを返します。
initialGame :: MonadRandom m => m Game
initialGame = Game initialBoard <$> initialHands

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
nextHand :: Game -> WithInvalid Tile
nextHand (Game _ []) = Left NoNextHand
nextHand (Game _ (hand : _)) = Right hand

laterHands :: Game -> WithInvalid [Tile]
laterHands (Game _ []) = Left NoNextHand
laterHands (Game _ (_ : rest)) = Right rest

rotateTile :: Rotation -> Tile -> Tile
rotateTile rotation (Tile number _) = Tile number rotation

type GameMove = (TilePos, Rotation)

tileMoveOf :: Tile -> GameMove -> TileMove
tileMoveOf tile (pos, rotation) = (pos, rotateTile rotation tile)

data GameState = GameState {board :: Board, hand :: Tile}
  deriving (Eq, Show)

-- ゲームからゲーム状況を取り出します。
-- ゲームにクリアしていて次に置くべきタイルがない場合は NoNextHand を返します。
gameStateOf :: Game -> WithInvalid GameState
gameStateOf game@(Game board _) = GameState board <$> nextHand game

applyMove' :: GameMove -> GameState -> WithInvalid Board
applyMove' move (GameState board hand) = putTile (tileMoveOf hand move) board

-- 指定された位置に置くべきタイルを置きます。
-- 不可能な操作をしようとした場合は、その原因を示すエラー値を返します。
applyMove :: GameMove -> Game -> WithInvalid Game
applyMove move game = make =<< applyMove' move =<< gameStateOf game
  where
    make board = Game board <$> laterHands game

possibleMoves' :: GameState -> [GameMove]
possibleMoves' (GameState board hand) = concatMap make $ rotatedTiles hand
  where
    make tile = zip (possiblePoss tile board) (repeat $ rotation tile)

-- 可能な手のリストを返します。
-- タイルに書かれた通路の対称性を考慮に入れるため、回転情報が違っていても見た目が同じになる手については、その片方のみがリストに含まれます。
possibleMoves :: Game -> [GameMove]
possibleMoves game = fromRight [] $ possibleMoves' <$> gameStateOf game

-- 可能な手とその手を実行した後の盤面から成るリストを返します。
-- possibleMoves' と applyMove' を別々に呼ぶよりも効率的です。
possibleMovesAndBoards' :: GameState -> [(GameMove, Board)]
possibleMovesAndBoards' (GameState board hand) = rights $ map make $ comb poss rotations
  where
    make move = (move, ) <$> putTileWithoutCheck (tileMoveOf hand move) board
    poss = adjacentPoss board
    rotations = distinctRotations hand

-- 可能な手とその手を実行した後のゲームから成るリストを返します。
-- possibleMoves と applyMove を別々に呼ぶよりも効率的です。
possibleMovesAndGames :: Game -> [(GameMove, Game)]
possibleMovesAndGames = error "not implemented"

-- ゲームをクリアしていれば True を返します。
isCleared :: Game -> Bool
isCleared (Game _ hands) = null hands

isOver' :: GameState -> Bool
isOver' (GameState board hand) = not $ any check $ rotatedTiles hand
  where
    check tile = canPutTileAnywhere tile board

-- 次に置くべきタイルを置ける場所がなく、これ以上ゲームを進められない場合に、True を返します。
isOver :: Game -> Bool
isOver game = fromRight False $ isOver' <$> gameStateOf game