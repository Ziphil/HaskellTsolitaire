--


module Ziphil.Util.Random where

import Data.List
import System.Random


-- 与えられた範囲に属する数をランダムに並び替えたリストを返します。
nubRandomRs :: RandomGen g => (Int, Int) -> g -> [Int]
nubRandomRs (low, high) gen = take (high - low + 1) $ nub $ randomRs (low, high) gen

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle gen list = map fst $ sortOn snd $ zip list $ nubRandomRs (1, length list) gen