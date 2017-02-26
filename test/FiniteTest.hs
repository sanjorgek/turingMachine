{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import qualified Data.Set                    as Set
import           Data.State
import           Math.Model.Automaton.Finite
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Variant

type Natural = Int

instance Variant Natural where
  invalid = do
    n <- arbitrary
    if n<0 then return n else return ((-1)*(n+1))
  valid = do
    n <- arbitrary
    if n>=0 then return n else return ((-1)*n)

instance (Arbitrary a, Variant a) => Arbitrary (State a) where
  arbitrary = do
    x <- valid
    return $ Q x

instance (Arbitrary a, Variant a, Enum a, Ord a) => Arbitrary (FiniteA a) where
  arbitrary = do
    d <- arbitrary
    nd <- arbitrary
    sqf <- arbitrary
    q0 <- arbitrary
    (oneof . map return) [F d sqf q0, FN nd sqf q0]

pairWord = F (liftDelta [(1,'0',1),(1,'1',2),(2,'0',2),(2,'1',1)]) (Set.fromList [Q 2]) (Q 2)

finiteAut = describe "Finite automaton check" $
  it "pair of one's" $ do
    checkString pairWord "" `shouldBe` True
    checkString pairWord "00000" `shouldBe` True
    checkString pairWord "00101" `shouldBe` True
    checkString pairWord "00001" `shouldBe` False
    checkString pairWord "11111" `shouldBe` False
    checkString pairWord "11011" `shouldBe` True

finiteAutEquiv = describe "Equiv" $
  prop "principal" $
    \fa w -> checkString (fa:: FiniteA Natural) w == checkString (convertFA fa) w


main::IO ()
main = hspec $ do
  describe "Math.Model.Automaton.Finite" finiteAut
  describe "Math.Model.Automaton.Finite" finiteAutEquiv
