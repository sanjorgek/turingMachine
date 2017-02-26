{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where

import qualified Data.Set                    as Set
import           Data.State
import           Math.Model.Automaton.Finite
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Variant

instance (Arbitrary a) => Arbitrary (State a) where
  arbitrary = do
    x <- arbitrary
    (oneof . map return) [QE, Q x]

instance (Ord a,Arbitrary a) => Variant (FiniteA a) where
  invalid = do
    nd <- arbitrary
    sqf <- arbitrary
    q0 <- arbitrary
    return $ FN nd sqf q0
  valid = do
    d <- arbitrary
    sqf <- arbitrary
    q0 <- arbitrary
    return $ F d sqf q0

instance (Ord a, Arbitrary a) => Arbitrary (FiniteA a) where
  arbitrary = do
    afn <- invalid
    af <- valid
    (oneof . map return) [afn, af]

pairWord = F (liftDelta [(1,'0',1),(1,'1',2),(2,'0',2),(2,'1',1)]) (Set.fromList [Q 2]) (Q 2)

finiteAut = describe "Finite automaton check" $
  it "pair of one's" $ do
    checkString pairWord "" `shouldBe` True
    checkString pairWord "00000" `shouldBe` True
    checkString pairWord "00101" `shouldBe` True
    checkString pairWord "00001" `shouldBe` False
    checkString pairWord "11111" `shouldBe` False
    checkString pairWord "11011" `shouldBe` True

reachTest = describe "Transform" $ do
  prop "reachable" $
    \ af w -> checkString (reachableDelta (af::FiniteA Int)) w == checkString af w
  prop "distinguishable" $
    \ af w -> checkString (distinguishableDelta (af::FiniteA Int)) w == checkString af w
  prop "minimize" $
    \ af w -> checkString (minimizeFinite (af::FiniteA Int)) w == checkString af w


main::IO ()
main = hspec $
  describe "Math.Model.Automaton.Finite" finiteAut
