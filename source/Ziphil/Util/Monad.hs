--


module Ziphil.Util.Monad where

import Control.Applicative
import Control.Monad
import Data.Bifunctor


infixr 1 <<
(<<) :: Monad m => m b -> m a -> m b
(<<) = flip (>>)

outA :: Applicative f => (f a, f b) -> f (a, b)
outA = uncurry $ liftA2 (,)

outFstA :: Applicative f => (f a, b) -> f (a, b)
outFstA = uncurry $ flip $ liftA . flip (,)

outSndA :: Applicative f => (a, f b) -> f (a, b)
outSndA = uncurry $ liftA . (,)

bimapSame :: Bifunctor p => (a -> b) -> p a a -> p b b
bimapSame = join bimap