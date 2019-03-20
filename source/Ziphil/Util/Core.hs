--


module Ziphil.Util.Core where


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