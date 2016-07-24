{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.Sigma
import qualified Data.Set as Set

enumWordTest = describe "enumWordTest" $ do
  alpF <- return (enumWord (Set.fromList ['a','b','c']))
  it "Empty word and empty alphabet" $
    enumWord Set.empty [] `shouldBe` 0
  it "Empty word and non-empty alphabet" $  
    alpF [] `shouldBe` 0
  it "Symbol word1" $  
    alpF "a" `shouldBe` 1
  it "Symbol word2" $  
    alpF "b" `shouldBe` 2
  it "Symbol word3" $  
    alpF "c" `shouldBe` 3
  it "Two Symbol word" $  
    alpF "aa" `shouldBe` 4
  it "Two Symbol word1" $  
    alpF "ab" `shouldBe` 5
  it "Two Symbol word2" $  
    alpF "bc" `shouldBe` 9
  it "Two Symbol word3" $  
    alpF "ca" `shouldBe` 10

main::IO ()
main = hspec $
  describe "Data.Sigma" enumWordTest
