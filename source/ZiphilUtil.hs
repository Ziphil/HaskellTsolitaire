--


module ZiphilUtil where

import Data.Array.IArray
import Data.Bifunctor
import Data.Ix


-- 文字列が与えられた長さになるように、その文字列の左側をスペースで埋めた文字列を返します。
-- 文字列が与えられた長さより長い場合は、そのまま返します。
pad :: Int -> String -> String
pad size string = replicate (size - length string) ' ' ++ string

interpose :: [a] -> [a] -> [a] -> [a]
interpose left right list = left ++ list ++ right

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation l m = length l == length m && all (flip elem l) m && all (flip elem m) l

-- 与えられたインデックスのリストに対して、そのインデックスの要素を全て集めたリストを返します。
infixl 9 !!&
(!!&) :: [a] -> [Int] -> [a]
list !!& indices = map (list !!) indices

infixl 9 !&
(!&) :: (IArray a e, IArray a i, Ix i, Ix j) => a i e -> a j i -> a j e
array !& indices = amap (array !) indices

liftMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
liftMaybe (Just s, Just t) = Just (s, t)
liftMaybe (_, _) = Nothing

liftFstEither :: (Either a b, c) -> Either a (b, c)
liftFstEither (Right s, t) = Right (s, t)
liftFstEither (Left s, _) = Left s

liftSndEither :: (c, Either a b) -> Either a (c, b)
liftSndEither (t, Right s) = Right (t, s)
liftSndEither (_, Left s) = Left s

bimapSame :: Bifunctor p => (a -> b) -> p a a -> p b b
bimapSame func = bimap func func