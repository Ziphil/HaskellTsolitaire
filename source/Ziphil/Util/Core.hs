--


module Ziphil.Util.Core where


infixl 7 #/
(#/) :: Integral a => a -> a -> a
(#/) = div

infixl 7 #%
(#%) :: Integral a => a -> a -> a
(#%) = mod

-- 2 変数関数と 1 変数関数を合成した関数を返します。
infixr 9 .^
(.^) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.^) = (.) . (.)

infixr 9 .^^
(.^^) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(.^^) = (.) . (.) . (.)