{-# OPTIONS_HADDOCK show-extensions #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Sigma
Description : Alphabet and symbols
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Alphabet and symbols of languaje
-}
module Data.Sigma
(
  -- * Symbols
	Symbol(..)
  ,Epsilon(..)
	,blank
	,Wd(..)
  -- * Alphabets
  ,Alphabet(..)
  ,enumWord
  ,closureAlph
  ,lessKWords
  ,kWords
) where
import           Data.Char
import           Data.List
import qualified Data.Map.Lazy as Map
import           Data.Monoid
import qualified Data.Set      as Set

{-|
Symbols are character, and with Unicode CharSet we have a big amount of them.
-}
type Symbol = Char

data Epsilon = Epsilon deriving(Show, Ord, Eq)

{-|
Symbol type are forced to be a monoid
-}
instance Monoid Symbol where
	mempty = blank

-- |Blank symbol
blank::Symbol
blank = '\164'

{-|
List symbol alias, Word are defined in Prelude
-}
type Wd = [Symbol]

-- |Simple word monoid, holds all properties
instance Monoid Wd where
  mempty = []
  mappend = (++)

-- |An alphabet is a set of symbols
type Alphabet = Set.Set Symbol

{-|
For every alphabet there is a function __h__ that maps one symbol to one
natural. For every __h__ function there is a function that enumerete every
words in that alphabet
-}
enumWord::Alphabet -> Wd -> Integer
enumWord sig w = let
    sigL = Set.toList sig
    n = genericLength sigL
    map = Map.fromList (zip (Set.toList sig) [1..])
    f [] = 0
    f xs = (n * f (init xs))+(map Map.! last xs)
  in f w

closureAlph' sigL = map (:"") sigL ++ [x:ys | ys<-closureAlph' sigL, x<-sigL]

{-|
Gives the Kleene Closure for all alphabets. closureAlph is a infinite list of
words.
-}
closureAlph::Alphabet -> [Wd]
closureAlph sig = "":closureAlph' (Set.toList sig)

{-|
For some alphabet __S__ and a natural number __n__ take all words of length
__n__ or less
-}
lessKWords::Alphabet -> Integer -> [Wd]
lessKWords sig k = let
    f x y = genericLength y <= x
  in
    takeWhile (f k) (closureAlph sig)

{-|
For some alphabet __S__ and a natural number __n__ take all words of length __n__
-}
kWords::Alphabet -> Integer -> [Wd]
kWords sig k = let
    f x y = genericLength y == x
    g x y = genericLength y < x
  in
    takeWhile (f k) (dropWhile (g k) (closureAlph sig))
