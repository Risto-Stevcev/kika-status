module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Unit (suite, test, timeout, describe, it)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.Unit.QuickCheck (quickCheck)

import Test.QuickCheck (Result(), (===))

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

main = runTest do
  it "the commutative property" do
    quickCheck theCommutativeProperty
