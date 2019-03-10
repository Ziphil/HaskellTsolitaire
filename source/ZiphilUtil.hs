--


module ZiphilUtil where

import Data.Bifunctor


pad :: Int -> String -> String
pad size list = replicate (size - length list) ' ' ++ list

interpose :: [a] -> [a] -> [a] -> [a]
interpose left right list = left ++ list ++ right

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation l m = length l == length m && all (flip elem l) m && all (flip elem m) l

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