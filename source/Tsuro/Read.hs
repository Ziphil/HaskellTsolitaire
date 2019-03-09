{-# LANGUAGE FlexibleInstances #-}


module Tsuro.Read where

import Tsuro.Base


class ReadRec a where
  readRec :: String -> Maybe a

instance ReadRec GameMove where
  readRec = undefined