{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.State
import Math.Model.Automaton.Finite
import qualified Data.Set as Set

pairWord = F (liftD [(1,'0',1),(1,'1',2),(2,'0',2),(2,'1',1)]) (Set.fromList [Q 2]) (Q 2)

finiteAut = describe "Finite automaton check" $ do
  it "pair of one's" $ do
    checkString pairWord "" `shouldBe` True
    checkString pairWord "00000" `shouldBe` True
    checkString pairWord "00101" `shouldBe` True
    checkString pairWord "00001" `shouldBe` False
    checkString pairWord "11111" `shouldBe` False
    checkString pairWord "11011" `shouldBe` True
    

main::IO ()
main = hspec $
  describe "Math.Model.Automaton.Finite" $
    finiteAut
