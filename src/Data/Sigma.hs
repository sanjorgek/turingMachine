{-# OPTIONS_HADDOCK show-extensions #-}
{-# OPTIONS_GHC -fno-warn-tabs      #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
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
import Data.List ( genericLength )
import qualified Data.Map.Lazy as Map
import           Data.Monoid
import qualified Data.Set      as Set
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup      as Semi
#endif

{-|
Symbols are character, and with Unicode CharSet we have a big amount of them.
-}
type Symbol = Char

#if MIN_VERSION_base(4,9,0)
instance Semi.Semigroup Symbol where
  (<>) a b  = toEnum $ (+) (fromEnum a) (fromEnum b)
#endif

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
    map' = Map.fromList (zip (Set.toList sig) [1..])
    f [] = 0
    f xs = (n * f (init xs))+(map' Map.! last xs)
  in f w

closureAlph' sigL = fmap (:"") sigL ++ [x:ys | ys<-closureAlph' sigL, x<-sigL]

{-|
Gives the Kleene Closure for all alphabets. closureAlph is a infinite list of
words.
-}
closureAlph::Alphabet -> [Wd]
closureAlph sig = if Set.null sig
  then [""]
  else "":closureAlph' (Set.toList sig)

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
