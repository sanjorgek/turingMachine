{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where

import qualified Data.Set              as Set
import           Data.Label
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance (Arbitrary a) => Arbitrary (Label a) where
  arbitrary = do
    x <- arbitrary
    (oneof . map return) [QE, Q x]

terminalTest = describe "terminal" $ do
  prop "Not in" $
    \x y -> (x == y) || not (terminal (Set.fromList [x]) (y:: Label Int))
  prop "In" $
    \x -> terminal (Set.fromList [x]) (x:: Label Int)

main::IO ()
main = hspec $
	describe "Data.State" terminalTest
