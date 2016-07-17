{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.State
import qualified Data.Set as Set

instance (Arbitrary a) => Arbitrary (State a) where
  arbitrary = do
    x <- arbitrary
    (oneof . map return) [QE, Q x]

terminalTest = describe "terminal" $ do
  prop "Not in" $
    \x y -> (x == y) || not (terminal (Set.fromList [Q x]) (Q y :: State Int))
  prop "In" $
    \x -> terminal (Set.fromList [Q x]) (Q x:: State Int)

main::IO ()
main = hspec $
	describe "Data.State.hs" terminalTest
