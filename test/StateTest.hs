{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where

import Test.Hspec
import Data.State

terminalTest = describe "terminal" $ do
  it "Not in" $
    terminal [Q 0] (Q 1) `shouldBe` False
  it "In" $
    terminal [Q 1] (Q 1) `shouldBe` True

main::IO ()
main = hspec $
	describe "Data.State.hs" $ do
    terminalTest
    