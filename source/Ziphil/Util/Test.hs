--


module Ziphil.Util.Test where

import Control.Exception.Base
import Test.Hspec


infix 1 ?==
(?==) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(?==) = shouldBe

infix 1 ?!=
(?!=) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(?!=) = shouldNotBe

infix 1 ?>>
(?>>) :: (HasCallStack, Show a) => a -> (a -> Bool) -> Expectation
(?>>) = shouldSatisfy

infix 1 ?!>
(?!>) :: (HasCallStack, Exception e) => IO a -> Selector e -> Expectation
(?!>) = shouldThrow