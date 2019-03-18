--


module Ziphil.Util.Random where

import Control.Monad.Random
import Data.List
import System.Random


-- 与えられた範囲に属する数をランダムに並び替えたリストを返します。
getNubRandomRs :: MonadRandom m => (Int, Int) -> m [Int]
getNubRandomRs (low, high) = take (high - low + 1) . nub <$> getRandomRs (low, high)

shuffle :: MonadRandom m => [a] -> m [a]
shuffle list = map fst . sortOn snd . zip list <$> getNubRandomRs (1, length list)

pick :: MonadRandom m => [a] -> m a
pick list = (list !!) <$> getRandomR (0, length list - 1)