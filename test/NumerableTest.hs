{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where

import qualified Data.Set              as Set
import           Data.Numerable
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

essenceOrdTest = describe "Essence Data Ord and EQ" $ do
  it "order 1" $
    (Empty == Occupied) `shouldBe` False
  it "order 2" $
    (Empty > Occupied) `shouldBe` False
  it "order 3" $
    (Empty < Occupied) `shouldBe` True
  it "order 4" $
    (Empty /= Occupied) `shouldBe` True
  it "order 5" $
    (Empty >= Occupied) `shouldBe` False
  it "order 6" $
    (Empty <= Occupied) `shouldBe` True

discreteOrdTest = describe "Discrete Data Ord and EQ" $ do
  prop "preserve order 1" $
    \ n m -> (n <= m) || (Fin n > Fin m)
  prop "preserve order 2" $
    \ n m -> (n >= m) || (Fin n < Fin m)
  prop "preserve order 3" $
    \ n m -> (n /= m) || (Fin n == Fin m)
  prop "preserve order 4" $
    \ n m -> (n == m) || (Fin n /= Fin m)
  prop "preserve order 5" $
    \ m -> Fin m < Numerable
  prop "preserve order 5" $
    \ m -> Fin m <= Numerable
  prop "preserve order 6" $
    \ m -> Fin m /= Numerable
  prop "preserve order 7" $
    \ m -> Numerable /= Fin m
  prop "preserve order 8" $
    \ m -> Numerable > Fin m
  prop "preserve order 9" $
    \ m -> Numerable >= Fin m
  it "preserve order 11" $
    compare Numerable Numerable `shouldBe` EQ
  prop "preserve order with bounds 1" $
    \n -> (n < 0) || minBound <= Fin n
  prop "preserve order with bounds 2" $
    \n -> Fin n < maxBound

main::IO ()
main = hspec $
  describe "Data.Numerable" $ do
    discreteOrdTest
    essenceOrdTest
