{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}


module ZiphilUtil.OverloadedLabels where

import Data.Proxy
import GHC.OverloadedLabels
import GHC.TypeLits


class Has (l :: Symbol) a b | a l -> b where
  from :: Proxy l -> a -> b

instance Has l a b => IsLabel l (a -> b) where
  fromLabel = from (Proxy :: Proxy l)