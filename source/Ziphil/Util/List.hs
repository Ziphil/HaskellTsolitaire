--


module Ziphil.Util.List where

import Control.Applicative
import Data.Function
import Data.List


-- 与えられたインデックスのリストに対して、そのインデックスの要素を全て集めたリストを返します。
infixl 9 !!&
(!!&) :: [a] -> [Int] -> [a]
(!!&) = map . (!!)

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