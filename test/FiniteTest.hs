{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Data.Numerable
    ( getNatural, Discrete(Numerable, Fin), Essence(Empty, Occupied) )
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import Data.Label ( Label(..) )
import Math.Model.Automaton.Finite
    ( automatonCardinality,
      automatonEssence,
      checkString,
      convertFA,
      distinguishableDelta,
      liftDelta,
      minimizeFinite,
      reachableDelta,
      FiniteA(..) )
import Test.Hspec ( hspec, describe, it, shouldBe )
import Test.Hspec.QuickCheck ( prop )
import Test.Hspec.Variant ()
import Test.QuickCheck ( oneof, Arbitrary(arbitrary) )
import Test.QuickCheck.Variant ( Variant(..) )

returnEnum = return . toEnum

oneOfEnum = oneof . fmap returnEnum

instance Variant () where
  invalid = return ()
  valid = return ()

instance Variant Char where
  invalid = oneOfEnum $ [0..31]++[127..1114111]
  valid = oneOfEnum [32..126]

instance (Arbitrary a) => Variant (Label a) where
  invalid = return QE
  valid = do
    x <- arbitrary
    return $ Q x

instance (Variant a) => Variant [a] where
  valid = do
    x <- valid
    xs <- valid
    (oneof . fmap return) [x:xs, []]
  invalid = do
    x <- invalid
    xs <- invalid
    y <- valid
    ys <- valid
    (oneof . fmap return) [[x], x:xs, x:ys, y:xs]

instance (Arbitrary a) => Arbitrary (Label a) where
  arbitrary = oneof [invalid, valid]

instance (Variant a, Variant b) => Variant ((,) a b) where
  invalid = do
    x <- invalid
    y <- invalid
    z <- valid
    w <- valid
    (oneof . fmap return) [(x,y), (x,z), (w,y)]
  valid = do
    x <- valid
    y <- valid
    return (x, y)

instance (Ord a, Variant a) => Variant (Set.Set a) where
  invalid = do
    xs <- invalid
    return $ Set.fromList xs
  valid = do
    xs <- valid
    (oneof . fmap return) [Set.empty, Set.fromList xs]

instance (Ord k, Variant k, Variant a) => Variant (Map.Map k a) where
  invalid = do
    xs <- invalid
    return $ Map.fromList xs
  valid = do
    xs <- valid
    (oneof . fmap return) [Map.empty, Map.fromList xs]

instance (Ord a,Arbitrary a) => Variant (FiniteA a) where
  invalid = do
    nd <- valid
    sqf <- valid
    q0 <- valid
    return $ FN nd sqf q0
  valid = do
    d <- valid
    sqf <- valid
    q0 <- valid
    return $ F d sqf q0

instance (Ord a, Arbitrary a) => Arbitrary (FiniteA a) where
  arbitrary = do
    afn <- invalid
    af <- valid
    (oneof . fmap return) [afn, af]

pairWord = F (liftDelta [(1,'0',1),(1,'1',2),(2,'0',2),(2,'1',1)]) (Set.fromList [Q 2]) (Q 2)

emptyLang1 = F (liftDelta [(1,'0',1),(1,'1',2),(2,'0',2),(2,'1',1)]) (Set.fromList [Q 3]) (Q 2)

finiteLang = F (liftDelta []) (Set.fromList [Q 2]) (Q 2)

finiteAut = describe "Finite automaton check" . it "pair of one's" $ do
    checkString pairWord "" `shouldBe` True
    checkString pairWord "00000" `shouldBe` True
    checkString pairWord "00101" `shouldBe` True
    checkString pairWord "00001" `shouldBe` False
    checkString pairWord "11111" `shouldBe` False
    checkString pairWord "11011" `shouldBe` True

transDetTest = describe "Transform" $ do
  prop "reachable check same" $
    \ af w -> checkString (reachableDelta (af::FiniteA Int)) w == checkString af w
  prop "distinguishable check same" $
    \ af w -> checkString (distinguishableDelta (af::FiniteA Int)) w == checkString af w
  prop "minimize check same" $
    \ af w -> checkString (minimizeFinite (af::FiniteA Int)) w == checkString af w
  prop "minimize" $
    \ af -> let naf = minimizeFinite (af::FiniteA Int) in minimizeFinite naf == naf
  prop "equivalence" $
    \fa w -> checkString (fa:: FiniteA Int) w == checkString (convertFA fa) w

cardinalityTest = describe "Cardinal" $ do
  it "essence" $ do
    automatonEssence pairWord `shouldBe` Occupied
    automatonEssence emptyLang1 `shouldBe` Empty
    automatonEssence finiteLang `shouldBe` Occupied
  it "cardinality" $ do
    automatonCardinality pairWord `shouldBe` Numerable
    automatonCardinality emptyLang1 `shouldBe` Fin 0
    automatonCardinality finiteLang `shouldBe` Fin 1
  prop "if empty then Fin 0" $
    \ af -> let
        e = automatonEssence (af:: FiniteA Int)
        c = automatonCardinality af
      in (e /= Empty) || (c == Fin 0)
  prop "if (Fin n) where n>0 then Occupied" $
    \ af -> let
        e = automatonEssence (af:: FiniteA Int)
        c = automatonCardinality af
      in (c /= Numerable) || (getNatural c == 0) || (e == Occupied)
  prop "if Numerable then Occupied" $
    \ af -> let
        e = automatonEssence (af:: FiniteA Int)
        c = automatonCardinality af
      in (c /= Numerable) || (e == Occupied)
  prop "if Numerable then not Empty" $
    \ af -> let
        e = automatonEssence (af:: FiniteA Int)
        c = automatonCardinality af
      in (c /= Numerable) || (e /= Empty)


main::IO ()
main = hspec . describe "Math.Model.Automaton.Finite" $ do
    finiteAut
    transDetTest
    cardinalityTest
