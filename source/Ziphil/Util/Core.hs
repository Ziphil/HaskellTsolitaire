--


module Ziphil.Util.Core where

import Control.Applicative
import Control.Monad
import Data.Array.IArray
import Data.Bifunctor
import Data.Function
import Data.Ix
import Data.List


-- 2 変数関数と 1 変数関数を合成した関数を返します。
infixr 9 .^
(.^) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.^) = (.) . (.)

infixr 9 .^^
(.^^) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(.^^) = (.) . (.) . (.)

-- 文字列が与えられた長さになるように、その文字列の左側をスペースで埋めた文字列を返します。
-- 文字列が与えられた長さより長い場合は、そのまま返します。
pad :: Int -> String -> String
pad size string = replicate (size - length string) ' ' ++ string

maximumBy' :: (a -> a -> Ordering) -> [a] -> (Int, a)
maximumBy' comp = maximumBy (on comp snd) . zip [0 ..]

minimumBy' :: (a -> a -> Ordering) -> [a] -> (Int, a)
minimumBy' comp = minimumBy (on comp snd) . zip [0 ..]

-- 与えられたインデックスの値を新しい値に置き換えたリストを返します。
-- インデックスは 0 以上かつリストの長さ未満でなければならず、そうでなかった場合の動作は未定義です。
update :: Int -> a -> [a] -> [a]
update _ _ [] = []
update i next (x : xs) =
  if i == 0
    then next : xs
    else x : update (i - 1) next xs

interpose :: [a] -> [a] -> [a] -> [a]
interpose left right list = left ++ list ++ right

-- 与えられた 2 つのリストの要素の全ての組み合わせから成るリストを返します。
comb :: [a] -> [b] -> [(a, b)]
comb = liftA2 (,)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation l m = length l == length m && all (flip elem l) m && all (flip elem m) l

-- 与えられたインデックスのリストに対して、そのインデックスの要素を全て集めたリストを返します。
infixl 9 !!&
(!!&) :: [a] -> [Int] -> [a]
(!!&) = map . (!!)

infixl 9 !&
(!&) :: (IArray a e, IArray a i, Ix i, Ix j) => a i e -> a j i -> a j e
(!&) = amap . (!)

outA :: Applicative f => (f a, f b) -> f (a, b)
outA = uncurry $ liftA2 (,)

outFstA :: Applicative f => (f a, b) -> f (a, b)
outFstA = uncurry $ flip $ liftA . flip (,)

outSndA :: Applicative f => (a, f b) -> f (a, b)
outSndA = uncurry $ liftA . (,)

bimapSame :: Bifunctor p => (a -> b) -> p a a -> p b b
bimapSame = join bimap