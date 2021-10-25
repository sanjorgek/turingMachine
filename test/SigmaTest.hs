{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where

import qualified Data.Set              as Set
import Data.Sigma ( enumWord )
import Test.Hspec ( hspec, describe, it, shouldBe )
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

enumWordTest = describe "enumWordTest" $ do
  let alpF = enumWord (Set.fromList ['a','b','c'])
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
