{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Cardinal
Description : Cardinal Def
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2016
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Cardinal definitions
-}
module Data.Numerable where

{-|
All sets can be one and only one:

- a empty set
- a set with, at least, one element
-}
data Essence = Empty | Occupied deriving(Show, Eq, Ord, Bounded)

{-|
Simple cardinality definition, we work here with numerable sets.

All numerable set have one and only one:

1. A finite size

2. A infinite size
-}
data Discrete = Fin Integer | Numerable deriving(Show, Eq)

{-|
Order for numerable cardinality
-}
instance Ord Discrete where
  compare (Fin n) (Fin m)     = compare n m
  compare Numerable Numerable = EQ
  compare Numerable _         = GT
  compare _ _                 = LT

{-|
Bound limits for numerable cardinality
-}
instance Bounded Discrete where
  minBound = Fin 0
  maxBound = Numerable

getNatural :: Discrete -> Integer
getNatural (Fin n) = n
getNatural _ = error "Not possible to extract the natural because is not a finit number"
