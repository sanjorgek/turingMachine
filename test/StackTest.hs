{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where

import           Data.State
import           Math.Model.Automaton.Stack
import qualified Data.Set                    as Set
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.Variant
import           Test.QuickCheck
import           Test.QuickCheck.Variant

anbn = Stack (liftDelta [(0,"a",'Z',0,"A"),(0,"a",'A',0,"AA"),(0,"b",'A',1,""),(1,"b",'A',1,"")]) (Q 0) Set.empty 'Z'

stackAut = describe "Stack automaton check" $
  it "a^nb^n" $ do
    checkWordByStack anbn "" `shouldBe` False
    checkWordByStack anbn "a" `shouldBe` False
    checkWordByStack anbn "ab" `shouldBe` True
    checkWordByStack anbn "aabb" `shouldBe` True

main::IO ()
main = hspec $
  describe "Math.Model.Automaton.Stack" $ do
    stackAut
