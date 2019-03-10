--


module ZiphilUtil.Random where

import Data.List
import System.Random


-- 与えられた範囲に属する数をランダムに並び替えたリストを返します。
nubRandomRs :: RandomGen g => (Int, Int) -> g -> [Int]
nubRandomRs (low, high) gen = take (high - low + 1) $ nub $ randomRs (low, high) gen